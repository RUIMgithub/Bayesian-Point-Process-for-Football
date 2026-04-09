############################################################
# Posterior predictive simulation of the 2022/2023 season
############################################################

data_foot <- readRDS("data_foot.rds")
samples <- readRDS("nimble_mcmc_posterior_results.rds")

library(dplyr)
library(ggplot2)

set.seed(123)

############################################################
# SETTINGS
############################################################

nTeams <- 18
nRounds <- 34
nIntervals <- 15
nSim <- 1000

tau <- seq(0,90,length.out=nIntervals+1)
delta <- diff(tau)

############################################################
# POSTERIOR SAMPLES
############################################################

mcmc_mat <- as.matrix(samples$samples)
nPost <- nrow(mcmc_mat)

############################################################
# FIXTURES (from dataset)
############################################################

fixtures <- data.frame(
  round = matches_raw$Round,
  home  = matches_raw$index_home,
  away  = matches_raw$index_away
)

############################################################
# MATCH SIMULATION
############################################################

simulate_match <- function(i,j,lambda0,alpha,h){

  goals_home <- 0
  goals_away <- 0

  for(k in 1:nIntervals){

    lambda_home <- lambda0[i,k] * exp(alpha[j] + h)
    lambda_away <- lambda0[j,k] * exp(alpha[i])

    goals_home <- goals_home + rpois(1, lambda_home * delta[k])
    goals_away <- goals_away + rpois(1, lambda_away * delta[k])
  }

  c(goals_home,goals_away)
}

############################################################
# SEASON SIMULATION
############################################################

simulate_season <- function(){

  s <- sample(1:nPost,1)

  h <- mcmc_mat[s,"home_advantage"]

  alpha <- mcmc_mat[s,grep("^def",colnames(mcmc_mat))]

  lambda0 <- matrix(NA,nTeams,nIntervals)

  for(i in 1:nTeams){
    for(k in 1:nIntervals){
      lambda0[i,k] <- mcmc_mat[s,paste0("dR0[",i,", ",k,"]")]
    }
  }

  points <- matrix(0,nTeams,nRounds)

  for(r in 1:nRounds){

    matches <- fixtures %>% filter(round==r)

    for(m in 1:nrow(matches)){

      i <- matches$home[m]
      j <- matches$away[m]

      res <- simulate_match(i,j,lambda0,alpha,h)

      gh <- res[1]
      ga <- res[2]

      if(gh>ga){
        points[i,r] <- points[i,r] + 3
      } else if(ga>gh){
        points[j,r] <- points[j,r] + 3
      } else {
        points[i,r] <- points[i,r] + 1
        points[j,r] <- points[j,r] + 1
      }

    }

  }

  apply(points,1,cumsum)
}

############################################################
# RUN SEASON SIMULATIONS
############################################################

points_array <- array(0,c(nTeams,nRounds,nSim))

for(s in 1:nSim){
  points_array[,,s] <- simulate_season()
}

############################################################
# EXPECTED POINTS
############################################################

expected_points <- apply(points_array,c(1,2),mean)

############################################################
# EXPECTED RANKINGS
############################################################

rankings <- matrix(NA,nTeams,nRounds)

for(r in 1:nRounds){
  rankings[,r] <- rank(-expected_points[,r],ties.method="first")
}

############################################################
# DATAFRAME FOR PLOTTING
############################################################

df_rank <- data.frame()

for(i in 1:nTeams){

  df_rank <- rbind(df_rank,
                   data.frame(
                     Team=i,
                     Round=1:nRounds,
                     Rank=rankings[i,]
                   ))
}

############################################################
# PLOT RANK EVOLUTION
############################################################

p <- ggplot(df_rank,
            aes(Round,Rank,group=Team,color=factor(Team))) +
  geom_line(linewidth=0.8) +
  scale_y_reverse() +
  theme_bw() +
  labs(x="Round",
       y="League position",
       color="Team")

print(p)

############################################################
# SAVE FIGURE
############################################################
#
#ggsave("simulated_rankings.pdf",
#       p,
#       width=8,
#       height=5)
       
       








############################################################
# Simulated goal-time distributions for Benfica vs Porto
############################################################

library(ggplot2)

# ----------------------------------------------------------
# Team indices
benfica <- 6
porto   <- 14

nSim <- 50000

# ----------------------------------------------------------
# Simulation storing goal times
simulate_match_times <- function(home, away, nSim=50000){

  goal_times_home <- vector("list", nSim)
  goal_times_away <- vector("list", nSim)

  score_home <- numeric(nSim)
  score_away <- numeric(nSim)

  for(s in 1:nSim){

    g_home <- c()
    g_away <- c()

    for(k in 1:15){

      lam_home <- lambda0[home,k] * exp(alpha[away] + h)
      lam_away <- lambda0[away,k] * exp(alpha[home])

      nh <- rpois(1, lam_home)
      na <- rpois(1, lam_away)

      if(nh>0){
        g_home <- c(g_home, runif(nh, tau[k], tau[k+1]))
      }

      if(na>0){
        g_away <- c(g_away,
                    runif(na, tau[k], tau[k+1]))
      }

    }

    goal_times_home[[s]] <- g_home
    goal_times_away[[s]] <- g_away

    score_home[s] <- length(g_home)
    score_away[s] <- length(g_away)

  }

  list(home_times = goal_times_home,
       away_times = goal_times_away,
       home_score = score_home,
       away_score = score_away)
}

# ----------------------------------------------------------
# Run simulation
sim <- simulate_match_times(benfica, porto, nSim)

# ----------------------------------------------------------
# Function to extract goal times for scorelines
# ----------------------------------------------------------
# Extract 1-0, 0-1, 1-1
get_times <- function(sim, h, a){

  idx <- which(sim$home_score==h & sim$away_score==a)

  home_times <- unlist(sim$home_times[idx])
  away_times <- unlist(sim$away_times[idx])

  data.frame(
    Time = c(home_times, away_times),
    GoalOrder = c(rep("Home goal", length(home_times)),
                  rep("Away goal", length(away_times))),
    Score = paste0(h,"-",a)
  )
}

# ----------------------------------------------------------
# Extract 1-0, 0-1, 1-1
df10 <- get_times(sim,1,0)
df01 <- get_times(sim,0,1)
df11 <- get_times(sim,1,1)

# ----------------------------------------------------------
# ----------------------------------------------------------
# Extract ordered goals for 2-0 and 0-2
extract_two_goal_times <- function(sim, h, a){

  idx <- which(sim$home_score==h & sim$away_score==a)

  first <- c()
  second <- c()

  for(i in idx){

    if(h==2){
      g <- sort(sim$home_times[[i]])
    } else {
      g <- sort(sim$away_times[[i]])
    }

    if(length(g)>=2){
      first  <- c(first, g[1])
      second <- c(second, g[2])
    }

  }

  data.frame(
    Time = c(first, second), GoalOrder = rep(c("First goal","Second goal"),
                    c(length(first),length(second))),
    Score = paste0(h,"-",a)
  )
}

df10 <- get_times(sim,1,0)
df01 <- get_times(sim,0,1)
df11 <- get_times(sim,1,1)

df20 <- extract_two_goal_times(sim,2,0)
df02 <- extract_two_goal_times(sim,0,2)


# ----------------------------------------------------------
# Combine datasets
df_all <- rbind(df10, df01, df11, df20, df02)

df_all$Score <- factor(df_all$Score,
                       levels = c("1-0","0-1","1-1",
                                  "2-0","0-2"))
                                  
# ----------------------------------------------------------
blank_row <- data.frame(
  Time = NA,
  GoalOrder = NA,
  Score = "blank"
)

df_plot <- rbind(df_all, blank_row)

# Plot
p <- ggplot(df_plot, aes(Time, fill = GoalOrder, color = GoalOrder)) +
  geom_density(alpha = 0.35, linewidth = 1, na.rm = TRUE) +
  facet_wrap(~Score, ncol = 3) +
  scale_fill_manual(values = c(
    "Home goal"="steelblue",
    "Away goal"="red",
    "First goal"="steelblue",
    "Second goal"="red"
  ), na.translate = FALSE) +
  scale_color_manual(values = c(
    "Home goal"="steelblue",
    "Away goal"="red",
    "First goal"="steelblue",
    "Second goal"="red"
  ), na.translate = FALSE) +
  labs(x = "Time (minutes)",
       y = "Density",
       fill = "Goal type",
       color = "Goal type") +
theme_bw() +
theme(
  strip.background = element_rect(fill = "white", color = "black"),
  strip.text = element_text(size = 11),
  panel.background = element_rect(fill = "white"),
  plot.background  = element_rect(fill = "white", color = NA),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white"),
  panel.grid = element_blank() )
  
print(p)

# ----------------------------------------------------------
# Save figure
ggsave("goal_time_simulation_benfica_porto.pdf",
       p,
       width=8,
       height=5)
       
       
       
       
       

library(ggplot2)
library(patchwork)

############################################################
# TOP PANELS (1-0, 0-1, 1-1)
df_top <- subset(df_all, Score %in% c("1-0","0-1","1-1"))

p_top <- ggplot(df_top, aes(Time, linetype = GoalOrder)) +
  geom_density(linewidth = 1, colour = "black") +
  facet_wrap(~Score, ncol = 3) +
  scale_linetype_manual(values = c("Home goal" = "solid", "Away goal" = "dashed" )) +
  labs(x = "Time (minutes)", y = "Density", linetype = "Goal") +
  theme_bw() +
  theme(legend.position = "right", strip.background = element_rect(fill = "white"), panel.grid = element_blank() )
    
    
############################################################
# BOTTOM PANELS (2-0, 0-2)
df_bottom <- subset(df_all, Score %in% c("2-0","0-2"))


  p_bottom <- ggplot(df_bottom,
                   aes(Time, linetype = GoalOrder)) +
  geom_density(linewidth = 1, colour = "black") +
  facet_wrap(~Score, ncol = 2) +
  scale_linetype_manual(values = c("First goal"  = "solid", "Second goal" = "dotted" )) +
  labs(x = "Time (minutes)", y = "Density", linetype = "Goal order") +
  theme_bw() +
  theme(legend.position = "right", strip.background = element_rect(fill = "white"), panel.grid = element_blank())

############################################################
# COMBINE PANELS

final_plot <- p_top / p_bottom

print(final_plot)

# ----------------------------------------------------------
## Save figure
#ggsave("goal_time_simulation_benfica_porto.pdf",
#       final_plot,
#       width=8,
#       height=5)
       
       
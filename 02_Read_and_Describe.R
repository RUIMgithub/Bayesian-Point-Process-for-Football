
setwd("D://Research//Papers_ComputModels//2026_foot_Daniel")
data_foot <- readRDS("data_foot.rds")

library(ggplot2)


# Matrix of results
table(data_foot$res_home, data_foot$res_away)


# Matrix of number of seasons per team
library(dplyr)
library(lubridate)
# convert Date to Date class if needed
data_foot$Date <- dmy(data_foot$Date)
# create season variable (e.g. 2010/2011)
data_foot <- data_foot %>%
  mutate(
    season_start = ifelse(month(Date) >= 7, year(Date), year(Date) - 1),
    season = paste0(season_start, "/", season_start + 1)
  )

# reshape matches so each team appears once per match
team_matches <- bind_rows(
  data_foot %>% select(team = team_home, season),
  data_foot %>% select(team = team_away, season)
)

# count seasons per team
seasons_per_team <- team_matches %>%
  distinct(team, season) %>%
  count(team, name = "n_seasons") %>%
  arrange(desc(n_seasons))
seasons_per_team




# (All) Goal times distribution - 5min intervals
all_home_times <- unlist(data_foot$time_home)
all_away_times <- unlist(data_foot$time_away)
hist(c(all_home_times, all_away_times),
     breaks = 30,
     main = "Distribution of goal times",
     xlab = "Minute")
ggsave("plot_df_kappas_men.pdf", plot = plot_df_kappas_men, width = 8, height = 4, device = "pdf")
     
     
# (All) Goal times distribution - 6min intervals with rounding at 45' and 90'     
all_home_times <- unlist(data_foot$time_home)
all_away_times <- unlist(data_foot$time_away)
all_times <- c(all_home_times, all_away_times)
# truncate added time to 90
all_times[all_times > 90] <- 90
# intervals of length 6
breaks <- seq(0, 90, by = 6)

#pdf("all_goals_histogram.pdf", width = 6, height = 4)
hist(all_times,
     breaks = breaks,
     #main = "Distribution of goal times",
     main="",
     xlab = "Minutes",
     ylab="Number of Goals",
     right = FALSE)
#dev.off()    
     
     
## (First goal) Goal times distribution - 6min intervals with rounding at 45' and 90'
# extract first goal time in each match
first_goal <- sapply(seq_len(nrow(data_foot)), function(i) {
  times <- c(data_foot$time_home[[i]], data_foot$time_away[[i]])
  if(length(times) == 0) return(NA)   # matches with no goals
  min(times)
})
# remove matches with no goals
first_goal <- first_goal[!is.na(first_goal)]
# truncate added time
first_goal[first_goal > 90] <- 90
# intervals of length 6
breaks <- seq(0, 90, by = 6)

#pdf("first_goal_histogram.pdf", width = 6, height = 4)
hist(first_goal,
     breaks = breaks,
     #main = "Distribution of the first goal time",
     main="",
     xlab = "Minutes",
     ylab="Number of Goals",
     right = FALSE)
#dev.off()



## (Time between 1st and 2nd goals) - 6min intervals
# extract time difference between first two goals
time_12 <- sapply(seq_len(nrow(data_foot)), function(i) {
  times <- c(data_foot$time_home[[i]], data_foot$time_away[[i]])
  if(length(times) < 2) return(NA)   # matches with fewer than two goals
  times <- sort(times)
  times[2] - times[1]
})

# remove matches with fewer than two goals
time_12 <- time_12[!is.na(time_12)]
# truncate differences larger than 90 (rare but safe)
time_12[time_12 > 90] <- 90
# intervals of length 6
breaks <- seq(0, 90, by = 6)

#pdf("time_between_first_two_goals_histogram.pdf", width = 6, height = 4)
hist(time_12,
     breaks = breaks,
     main = "",
     xlab = "Minutes",
     ylab="Number of Goals",
     right = FALSE)
#dev.off()
   
   
   
## Histograms of goal-scoring times for selected teams (separate PDFs)
teams <- c(6, 14, 26, 17, 9, 32)
names(teams) <- c("Benfica", "FC_Porto", "Portimonense",
                  "Gil_Vicente", "BSAD", "Uniao_da_Madeira")
breaks <- seq(0, 90, by = 6)
for(k in seq_along(teams)){
  team_id <- teams[k]
  times <- c()
  for(i in seq_len(nrow(data_foot))){
    if(data_foot$index_home[i] == team_id){
      times <- c(times, data_foot$time_home[[i]])
    }
    if(data_foot$index_away[i] == team_id){
      times <- c(times, data_foot$time_away[[i]])
    }
  }
  times <- times[!is.na(times)]
  times[times > 90] <- 90
  pdf(paste0(names(teams)[k], "_goal_times_histogram.pdf"),
      width = 6, height = 4)
  hist(times,
       breaks = breaks,
       main = "",
       xlab = "Minutes",
       right = FALSE)

  dev.off()
}






######################################
## Number of goals scored for three groups of teams
###################

library(dplyr)
library(tidyr)
library(ggplot2)

# teams to include
teams <- c("Benfica","Braga","FC Porto","Ferreira",
           "Guimaraes","Maritimo","Nacional","Rio Ave","Sporting")
# interval boundaries
tau <- seq(0,90,by=6)
# labels for the x-axis
interval_labels <- paste0("[",tau[-length(tau)],",",tau[-1],"]")
# dataframe to store results
results <- data.frame()
for(team in teams){
  times <- c()
  for(i in seq_len(nrow(data_foot))){
    if(data_foot$team_home[i] == team){
      times <- c(times, data_foot$time_home[[i]])
    }
    if(data_foot$team_away[i] == team){
      times <- c(times, data_foot$time_away[[i]])
    }
  }
  times <- times[!is.na(times)]
  times[times > 90] <- 90
  interval <- cut(times,
                  breaks = tau,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = interval_labels)
  counts <- table(interval)
  df_tmp <- data.frame(
    Team = team,
    Interval = names(counts),
    Goals = as.numeric(counts)
  )
  results <- rbind(results, df_tmp)
}

# plot
pp <- ggplot(results,
             aes(x = Interval, y = Goals, group = Team, color = Team)) +
  geom_line() +
  geom_point() +
  labs(x = "Time Intervals", y = "Number of Goals") +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
pp
#ggsave("goal_times_by_team.pdf",
#       plot = pp,
#       width = 6,
#       height = 4,
#       device = "pdf")

       
       
######################################
## Number of goals scored for other teams
# teams for the second figure
teams <- c("Academica","Arouca","Belenenses","Boavista",
           "Chaves","Estoril","Famalicao","Feirense",
           "Gil Vicente","Moreirense","Olhanense",
           "Portimonense","Santa Clara","Setubal","Tondela")

# 6-minute intervals
tau <- seq(0,90,by=6)
interval_labels <- paste0("[",tau[-length(tau)],",",tau[-1],"]")
results2 <- data.frame()
for(team in teams){
  times <- c()
  for(i in seq_len(nrow(data_foot))){
    if(data_foot$team_home[i] == team){
      times <- c(times, data_foot$time_home[[i]])
    }
    if(data_foot$team_away[i] == team){
      times <- c(times, data_foot$time_away[[i]])
    }
  }
  times <- times[!is.na(times)]
  times[times > 90] <- 90
  interval <- cut(times,
                  breaks = tau,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = interval_labels)
  counts <- table(interval)
  df_tmp <- data.frame(
    Team = team,
    Interval = names(counts),
    Goals = as.numeric(counts)
  )
  results2 <- rbind(results2, df_tmp)
}

# plot
p2 <- ggplot(results2,
             aes(x = Interval, y = Goals, group = Team, color = Team)) +
  geom_line() +
  geom_point() +
  labs(x = "Time Intervals", y = "Number of Goals") +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))
p2
# save figure
#ggsave("goal_times_other_teams.pdf",
#       plot = p2,
#       width = 6,
#       height = 4,
#       device = "pdf")

       

######################################
## Number of goals scored for smaller teams
# teams for the third figure
teams <- c("Aves","Beira Mar","BSAD","Farense",
           "Leiria","Naval","Penafiel",
           "U. Madeira","Vizela")
# 6-minute intervals
tau <- seq(0,90,by=6)
interval_labels <- paste0("[",tau[-length(tau)],",",tau[-1],"]")
results3 <- data.frame()
for(team in teams){
  times <- c()
  for(i in seq_len(nrow(data_foot))){
    if(data_foot$team_home[i] == team){
      times <- c(times, data_foot$time_home[[i]])
    }
    if(data_foot$team_away[i] == team){
      times <- c(times, data_foot$time_away[[i]])
    }
  }
  times <- times[!is.na(times)]
  times[times > 90] <- 90
  interval <- cut(times,
                  breaks = tau,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = interval_labels)
  counts <- table(interval)
  df_tmp <- data.frame(
    Team = team,
    Interval = names(counts),
    Goals = as.numeric(counts)
  )
  results3 <- rbind(results3, df_tmp)
}

# plot
p3 <- ggplot(results3,
             aes(x = Interval, y = Goals, group = Team, color = Team)) +
  geom_line() +
  geom_point() +
  labs(x = "Time Intervals", y = "Number of Goals") +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

p3

# save
#ggsave("goal_times_small_teams.pdf",
#       plot = p3,
#       width = 6,
#       height = 4,
#       device = "pdf")
#       
              
              


######################################
## Total number of goals at home and away
######################################

# goals scored at home
home_goals <- data_foot %>%
  group_by(team_home) %>%
  summarise(Home_Goals = sum(res_home)) %>%
  rename(Team = team_home)

# goals scored away
away_goals <- data_foot %>%
  group_by(team_away) %>%
  summarise(Away_Goals = sum(res_away)) %>%
  rename(Team = team_away)

# merge
goals <- full_join(home_goals, away_goals, by = "Team")

goals <- goals %>%
  mutate(Total = Home_Goals + Away_Goals) %>%
  arrange(desc(Home_Goals))
  
# reshape for plotting
goals_long <- goals %>%
  pivot_longer(cols = c(Home_Goals, Away_Goals),
               names_to = "Location",
               values_to = "Goals")
goals_long$Team <- factor(goals_long$Team, levels = goals$Team)

goals_long$Location <- factor(goals_long$Location,
                              levels = c("Home_Goals","Away_Goals"))
                              
# plot
p <- ggplot(goals_long,
            aes(x = Team, y = Goals, fill = Location)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(values = c("black","gray")) +
  labs(x = "", y = "Number of Goals") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p

# save
#ggsave("home_away_goals_by_team.pdf",
#       plot = p,
#       width = 8,
#       height = 4,
#       device = "pdf")
       
       
 


######################################
## Build outcome counts
######################################
library(patchwork)
# Determine match outcome
df_outcomes <- data_foot %>%
  mutate(
    home_win  = res_home > res_away,
    away_win  = res_home < res_away,
    draw      = res_home == res_away
  )

# Home outcomes
home_stats <- df_outcomes %>%
  group_by(team_home) %>%
  summarise(
    Home_Wins  = sum(home_win),
    Home_Draws = sum(draw),
    Home_Losses= sum(away_win)
  ) %>%
  rename(Team = team_home)

# Away outcomes
away_stats <- df_outcomes %>%
  group_by(team_away) %>%
  summarise(
    Away_Wins  = sum(away_win),
    Away_Draws = sum(draw),
    Away_Losses= sum(home_win)
  ) %>%
  rename(Team = team_away)

# Combine
team_stats <- left_join(home_stats, away_stats, by="Team")
team_stats

######################################
## Plot 1 – Wins
######################################

p1 <- ggplot(team_stats,
             aes(x = Home_Wins, y = Away_Wins, color = Team)) +
  geom_point(shape = 1, size = 2) +
  geom_abline(linetype = "dashed") +
  labs(x = "Home Wins", y = "Away Wins") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

######################################
## Plot 2 – Draws
######################################

p2 <- ggplot(team_stats,
             aes(x = Home_Draws, y = Away_Draws, color = Team)) +
  geom_point(shape = 1, size = 2) +
  geom_abline(linetype = "dashed") +
  labs(x = "Home Draws", y = "Away Draws") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

######################################
## Plot 3 – Losses
######################################

p3 <- ggplot(team_stats,
             aes(x = Home_Losses, y = Away_Losses, color = Team)) +
  geom_point(shape = 1, size = 2) +
  geom_abline(linetype = "dashed") +
  labs(x = "Home Losses", y = "Away Losses") +
  theme_bw() +
  guides(color = guide_legend(ncol = 1)) +
  theme_bw() +
  #theme(panel.grid = element_blank(), legend.position = "none")
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.3, "cm")
 )

######################################
## Combine plots
######################################

library(gridExtra)

ppp <- grid.arrange(p1, p2, p3, ncol = 3)
ppp

ggsave("home_vs_away_outcomes_legend.pdf",
       plot = ppp,
       width = 10,
       height = 3, device="pdf")      
       
       
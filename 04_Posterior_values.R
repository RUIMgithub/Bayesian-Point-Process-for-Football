# -------------------------------------------------
# Extract posterior summaries for selected parameters

library(coda)

# Need to run the model first in the 03_03_code_foot_Point_Process.R
samples <- readRDS("nimble_mcmc_posterior_results.rds")


# Convert MCMC output to matrix
mcmc_mat <- as.matrix(samples$samples)


# Function to compute posterior mean and 95% credible interval
post_summary <- function(param){
  
  vals <- mcmc_mat[, param]
  
  c(
    mean = mean(vals),
    q025 = quantile(vals, 0.025),
    q975 = quantile(vals, 0.975)
  )
}

# Parameters appearing in the table
params <- c(
  "def[6]",
  "def[8]",
  "def[14]",
  "def[22]",
  "def[25]",
  "def[30]",
  "def[31]",
  "home_advantage",
  "beta1",
  "beta2"
)

# Compute summaries
tab <- t(sapply(params, post_summary))

# Round values
tab <- round(tab, 3)
tab


## -------------------------------------------------
## Format numbers for LaTeX (remove leading zero)
#format_latex <- function(x){
#  
#  vals <- sprintf("%.3f", x)
#  
#  # remove leading zeros
#  vals <- sub("^0\\.", ".", vals)
#  vals <- sub("^-0\\.", "-.", vals)
#  
#  paste(vals, collapse = " & ")
#}

# -------------------------------------------------
# Print rows ready to paste into LaTeX
for(i in 1:nrow(tab)){
  cat(format_latex(tab[i,]), "\\\\\n")
}

# -------------------------------------------------
# Optional: view the summary table in R
print(tab)





# Posterior values:
# Get the intensity mean for each team per interval
team <- 9
posterior_means <- colMeans(as.matrix(samples$samples))
intsties <- posterior_means[grep(paste0("^dR0\\[", team, ","), names(posterior_means))]
mean_intsties <- round(mean(intsties),3)
mean_intsties
vals <- round(intsties, 2)
vals <- sub("^0\\.", ".", sprintf("%.2f", vals))

cat(paste(c(vals, mean_intsties), collapse = " & "))





# ---------------------------------------------------------
# Intensity plots: posterior mean goal-scoring intensities
# ---------------------------------------------------------

# Number of teams and number of time intervals
nTeams <- 33
nIntervals <- 15

# ---------------------------------------------------------
# 1. Reconstruct matrix of posterior mean intensities
# dR0_mat[i,k] = posterior mean intensity for team i in interval k

dR0_mat <- matrix(NA, nTeams, nIntervals)

for(i in 1:nTeams){
  for(k in 1:nIntervals){
    dR0_mat[i,k] <- posterior_means[paste0("dR0[", i, ", ", k, "]")]
  }
}


# ---------------------------------------------------------
# 2. Compute time midpoints of the intervals
# Intervals partition the match into 15 segments of 6 minutes

tau <- seq(0, 90, length.out = nIntervals + 1)

# Midpoint of each interval used as x-axis coordinate
time_mid <- (tau[-1] + tau[-length(tau)]) / 2


# ---------------------------------------------------------
# 3. Convert matrix to long format for plotting

library(dplyr)
library(tidyr)

df_plot <- as.data.frame(dR0_mat)
# Rename columns as interval identifiers
colnames(df_plot) <- paste0("I",1:nIntervals)
# Add team index
df_plot$Team <- factor(1:nTeams)
# Convert wide matrix to long format
df_plot <- df_plot %>%
  pivot_longer(-Team,
               names_to = "Interval",
               values_to = "Intensity")
# Convert interval labels to numeric
df_plot$Interval <- as.numeric(sub("I","",df_plot$Interval))
# Assign midpoint time corresponding to each interval
df_plot$Time <- time_mid[df_plot$Interval]


# ---------------------------------------------------------
# 4. Plot raw intensity curves (piecewise representation)

library(ggplot2)

p_line <- ggplot(df_plot, aes(x = Time, y = Intensity)) +
  # Draw intensity profile
  geom_line(color = "blue", linewidth = 0.6) +
  # One panel per team with independent y-scale
  facet_wrap(~Team, ncol = 6, scales = "free_y") +
  # Add team number inside each panel
  geom_text(data = df_plot %>% group_by(Team) %>% slice(1),
            aes(label = Team),
            x = 5, y = Inf,
            vjust = 1.2,
            size = 3,
            inherit.aes = FALSE) +
  labs(x = "Time (minutes)",
       y = "Goal-scoring intensity") +
  # White background
  theme_classic() +
  # Remove facet strip and draw panel border
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9)
  )

print(p_line)


# ---------------------------------------------------------
# 5. Plot smoothed intensity curves (LOESS smoothing)

p_smooth <- ggplot(df_plot, aes(x = Time, y = Intensity)) +
  # Smooth intensity trajectory
  geom_smooth(method = "loess",
              se = FALSE,
              span = 0.6,
              color = "blue",
              linewidth = 0.8) +
  facet_wrap(~Team, ncol = 6, scales = "free_y") +
  # Display team index inside panel
  geom_text(data = df_plot %>% group_by(Team) %>% slice(1),
            aes(label = Team),
            x = 5,
            y = Inf,
            vjust = 1.2,
            size = 3,
            inherit.aes = FALSE) +
  labs(x = "Time (minutes)",
       y = "Goal-scoring intensity") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9)
  )

print(p_smooth)


# ---------------------------------------------------------
# 6. Save the smoothed figure

ggsave("team_intensity_profiles.pdf",
       p_smooth,
       width = 12,
       height = 6)
       
       
       
       
       


# --------------------------------------------------
# Goal scoring intensity for 6 teams against strong, median and weak opponents

# Teams used in the figure

teams <- c(6,14,30,3,18,25)
team_names <- c("Benfica","FC Porto","Sporting",
                "BSAD","Naval","Penafiel")

names(teams) <- team_names

# --------------------------------------------------
# Opponent strength scenarios

alpha_vals <- c(-0.6,0,0.3)
alpha_labels <- c("Strong opponent","Average opponent","Weak opponent")

# home advantage posterior mean
h <- posterior_means["home_advantage"]

# --------------------------------------------------
# Extract baseline intensities for teams

nIntervals <- 15
lambda0 <- matrix(NA,length(teams),nIntervals)
for(i in seq_along(teams)){
  team_id <- teams[i]
  lambda0[i,] <- posterior_means[paste0("dR0[",team_id,", ",1:nIntervals,"]")]
}

# --------------------------------------------------
# Time midpoints
tau <- seq(0,90,length.out=nIntervals+1)
time_mid <- (tau[-1]+tau[-length(tau)])/2

# --------------------------------------------------
# Compute intensities
df <- data.frame()
for(i in seq_along(teams)){
  for(a in seq_along(alpha_vals)){
    lambda <- lambda0[i,] * exp(alpha_vals[a] + h)
    tmp <- data.frame(
      Team = names(teams)[i],
      Time = time_mid,
      Intensity = lambda,
      Opponent = alpha_labels[a]
    )
    df <- rbind(df,tmp)
  }
}
df$Team <- factor(df$Team,
                  levels = c("Benfica",
                             "FC Porto",
                             "Sporting",
                             "BSAD",
                             "Naval",
                             "Penafiel"))
df$Opponent <- factor(df$Opponent,
                      levels = c("Weak opponent",
                                 "Average opponent",
                                 "Strong opponent"))

# --------------------------------------------------
# Plot
p <- ggplot(df,
            aes(Time, Intensity, color = Opponent, shape = Opponent)) +
  geom_point(size = 0.8) +
  geom_smooth(aes(linetype = Opponent),
              se = FALSE,
              span = 0.6,
              method = "loess",
              linewidth = 0.7) +
  facet_wrap(~Team, ncol = 3, scales = "free_y") +
  labs(x = "Time (minutes)",
       y = "Goal-scoring intensity",
       color = "Opponent type",
       shape = "Opponent type",
       linetype = "Opponent type") +
  scale_color_manual(values = c("blue","red","green")) +
  scale_shape_manual(values = c(16,17,15)) +   # circle, triangle, square
  theme_bw()
print(p)

# --------------------------------------------------
# Save
ggsave("home_intensity_vs_opponent.pdf",
       p,
       width=10,
       height=4)
       
       
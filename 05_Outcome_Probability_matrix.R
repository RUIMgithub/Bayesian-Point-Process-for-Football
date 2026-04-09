############################################################
# Simulation of match score probabilities
# using posterior mean parameters
############################################################
# -------------------------------------------------
# Extract posterior summaries for selected parameters

library(coda)


setwd("D://Research//Papers_ComputModels//2026_foot_Daniel")
samples <- readRDS("nimble_mcmc_posterior_results.rds")



set.seed(123)

############################################################
# SETTINGS
############################################################

nIntervals <- 15
nSim <- 50000
maxGoals <- 9

tau <- seq(0,90,length.out=nIntervals+1)
delta <- diff(tau)

############################################################
# Compute posterior means from MCMC output
############################################################

# convert MCMC output to matrix
mcmc_mat <- as.matrix(samples$samples)



# posterior means
posterior_means <- colMeans(mcmc_mat)


############################################################
# EXTRACT POSTERIOR MEAN PARAMETERS
h <- posterior_means["home_advantage"]
alpha <- posterior_means[grep("^def", names(posterior_means))]
nTeams <- length(alpha)
lambda0 <- matrix(NA,nTeams,nIntervals)
for(i in 1:nTeams){
  for(k in 1:nIntervals){
    lambda0[i,k] <- posterior_means[paste0("dR0[",i,", ",k,"]")]
  }
}

############################################################
# FUNCTION TO SIMULATE ONE MATCH
############################################################

simulate_match <- function(i,j){
  g_home <- 0
  g_away <- 0

  for(k in 1:nIntervals){
    lambda_home <- lambda0[i,k] * exp(alpha[j] + h)
    lambda_away <- lambda0[j,k] * exp(alpha[i])

    g_home <- g_home + rpois(1, lambda_home)
    g_away <- g_away + rpois(1, lambda_away)
  }

  c(g_home,g_away)
}

############################################################
# FUNCTION TO BUILD SCORE PROBABILITY MATRIX
############################################################

score_matrix <- function(home,away){

  scores <- matrix(0,nSim,2)
  for(s in 1:nSim){
    scores[s,] <- simulate_match(home,away)
  }

  tab <- table(
    factor(scores[,1],levels=0:maxGoals),
    factor(scores[,2],levels=0:maxGoals)
  )
  print(tab)
  prob <- tab/nSim
  print(prob)
  round(prob,4)
}

############################################################
# EXAMPLE FIXTURES
############################################################

porto  <- 14
benfica <- 6
sporting <- 30
braga <- 8

bsad <- 9
Penafiel <- 25
Portimonense <- 26
GilVicente <- 17

############################################################
# COMPUTE MATRICES
############################################################

M1 <- score_matrix(porto,benfica)
M2 <- score_matrix(sporting,benfica)
M3 <- score_matrix(braga,sporting)
M4 <- score_matrix(benfica,porto)

M5 <- score_matrix(benfica,bsad)
M6 <- score_matrix(porto,Penafiel)
M7 <- score_matrix(Portimonense,braga)
M8 <- score_matrix(GilVicente,sporting)

##PRINT MATRICES

print(M1)
print(M2)
print(M3)
print(M4)

print(M5)
print(M6)
print(M7)
print(M8)


############################################################
# FUNCTION TO PRINT LATEX TABLE 
latex_matrix <- function(M){
  for(i in 1:nrow(M)){
    row <- paste(format(M[i,],nsmall=2),collapse=" & ")
    cat(i-1,"&",row,"\\\\\n")
  }
}

###### LATEX OUTPUT
latex_matrix(M1)




# --------------------------------------------------
# Function to compute outcome probabilities
# --------------------------------------------------

result_probs <- function(M){
  home_win <- sum(M[row(M) > col(M)])
  draw     <- sum(M[row(M) == col(M)])
  away_win <- sum(M[row(M) < col(M)])

  c(HomeWin = home_win,
    Draw    = draw,
    AwayWin = away_win)
}

# --------------------------------------------------
# Compute probabilities for the 8 games
games <- list(
  "FC Porto vs Benfica"     = M1,
  "Benfica vs FC Porto"     = M2,
  "Sporting vs Benfica"     = M3,
  "Braga vs Sporting"       = M4,
  "Benfica vs BSAD"         = M5,
  "FC Porto vs Penafiel"    = M6,
  "Portimonense vs Braga"   = M7,
  "Gil Vicente vs Sporting" = M8
)

results <- t(sapply(games, result_probs))
results <- round(results,3)
results











library(nimble)

setwd("D://Research//Papers_ComputModels//2026_foot_Daniel")
data_foot <- readRDS("data_foot.rds")


# Number of games and intervals
NN <- 3408     # Number of games
l  <- 15       # Number of time intervals
x  <- 90       # Total game duration in minutes
max_goals <- 10
eps <- 0.0001

# Initializing matrices and vectors
t_home <- matrix(0, nrow = NN, ncol = max_goals)   # goal times for the home team
t_away <- matrix(0, nrow = NN, ncol = max_goals)   # goal times for the away team
tau <- numeric(l + 1)
status_home <- array(0, dim = NN)
status_away <- array(0, dim = NN)
dNR_home <- matrix(0, nrow = NN, ncol = l)
dNR_away <- matrix(0, nrow = NN, ncol = l)
home_goals <- array(0, dim = NN)
away_goals <- array(0, dim = NN)
red_card_home <- matrix(0, nrow = NN, ncol = l)
red_card_away <- matrix(0, nrow = NN, ncol = l)

# Calculating tau outside the model
tau <- seq(0, x, length.out = l + 1)

# Defining the censorship vectors
status_home <- data_foot$ind_home[1:NN]
status_away <- data_foot$ind_away[1:NN]

# Defining team indices
team_home <- data_foot$index_home[1:NN]
team_away <- data_foot$index_away[1:NN]

# Defining red card matrices
red_card_home <- data_foot$red_home[1:NN]
red_card_home <- do.call(rbind, data_foot$red_home[1:NN])
red_card_away <- data_foot$red_away[1:NN]
red_card_away <- do.call(rbind, data_foot$red_away[1:NN])

# Defining t_home and t_away and computing increments
for (j in 1:NN) {

    # Number of goals
    home_goals[j] <- length(data_foot$time_home[[j]])
    away_goals[j] <- length(data_foot$time_away[[j]])

    # Home goals
    if (home_goals[j] > 0) {
        times_home <- data_foot$time_home[[j]]
        times_home[times_home > 90] <- 90
        times_home[times_home == 90] <- 90 - 1e-8      # avoid boundary issue
        #t_home[j, 1:home_goals[j]] <- times_home
        idx_home <- findInterval(times_home, tau)
        #idx_home <- idx_home[idx_home >= 1 & idx_home <= l]
        counts_home <- tabulate(idx_home, nbins = l)
        dNR_home[j, ] <- counts_home * status_home[j]
    }

    # Away goals
    if (away_goals[j] > 0) {
        times_away <- data_foot$time_away[[j]]
        times_away[times_away > 90] <- 90
        times_away[times_away == 90] <- 90 - 1e-8      # avoid boundary issue
        #t_away[j, 1:away_goals[j]] <- times_away
        idx_away <- findInterval(times_away, tau)
        #idx_away <- idx_away[idx_away >= 1 & idx_away <= l]
        counts_away <- tabulate(idx_away, nbins = l)
        dNR_away[j, ] <- counts_away * status_away[j]
    }
}

# Data for the model
data <- list(
    tau = tau,
    dNR_home = dNR_home,
    dNR_away = dNR_away,
    red_card_home = red_card_home,
    red_card_away = red_card_away
)

# Constants used in the model
constants <- list(
    NN = NN,
    l = l,
    e0 = 0.1,
    r0 = 0.06,
    team_home = team_home,
    team_away = team_away,
    num_teams = 33
)

# Initialization of model parameters
inits1 <- list(def_star = rep(-0.5, constants$num_teams),
               home_advantage = 0.1,
               dR0 = matrix(rep(0.05, constants$num_teams * l), nrow = constants$num_teams, ncol = l),
               mu.def = 1,
               tau.def = 0.001,
               beta1 = 0.01,
               beta2 = 0.01
)



# Definition of the NIMBLE model
code <- nimbleCode({

    # Priors
    for (i in 1:num_teams) {
        def_star[i] ~ dnorm(mu.def, tau.def)
    }

    mu.def ~ dnorm(0, 0.0001)
    tau.def ~ dgamma(0.01, 0.01)
    beta1 ~ dnorm(0, 0.001)
    beta2 ~ dnorm(0, 0.001)
    home_advantage ~ dnorm(0, 0.0001)

    for (i in 1:num_teams) {
        def[i] <- def_star[i] - mean(def_star[1:num_teams])
    }

    # Baseline intensities
    for (i in 1:num_teams) {
        for (k in 1:l) {
             dR0[i, k] ~ dgamma(mu[i, k], e0)
             mu[i, k] <- dR0_star[i, k] * e0
             dR0_star[i, k] <- r0 * (tau[k + 1] - tau[k])
        }
    }

    # Likelihood
    for (j in 1:NN) {
        for (k in 1:l) {
            # Home team
            dNR_home[j, k] ~ dpois(Irt_home[j, k])
            Irt_home[j, k] <- exp(def[team_away[j]] + home_advantage + beta1 * red_card_home[j, k]) *
                              dR0[team_home[j], k]

            # Away team
            dNR_away[j, k] ~ dpois(Irt_away[j, k])
            Irt_away[j, k] <- exp(def[team_home[j]] + beta2 * red_card_away[j, k]) *
                              dR0[team_away[j], k]
        }
    }
})



#--------------------------------------------------
# Create the NIMBLE model
model <- nimbleModel( code, constants = constants,
                      data = data, inits = inits1
)

# Optional check
model$calculate()

#--------------------------------------------------
# Configure the MCMC
conf <- configureMCMC( model,
                       monitors = c("def", "home_advantage", "beta1", "beta2", "dR0")
)

# Optional: inspect samplers
conf$printSamplers()

#--------------------------------------------------
# Build the MCMC algorithm
mcmc <- buildMCMC(conf)

#--------------------------------------------------
# Compile the model and MCMC (for speed)
Cmodel <- compileNimble(model)
Cmcmc  <- compileNimble(mcmc, project = model)

#--------------------------------------------------
# Run the MCMC
samples <- runMCMC(Cmcmc,
                   #niter = 100000,  # takes 2 days
                   niter = 1000,
                   #nburnin = 20000,
                   nburnin = 200,    # for niter = 100000
                   #thin = 10,
                   thin = 2,
                   nchains = 1,
                   samplesAsCodaMCMC = TRUE,
                   summary = TRUE
           )

saveRDS(samples, "nimble_mcmc_posterior_results.rds")


#--------------------------------------------------
# Inspect results
print(samples$summary)

# Traceplots
plot(samples$samples)

# Posterior means
posterior_means <- colMeans(as.matrix(samples$samples))
print(posterior_means)




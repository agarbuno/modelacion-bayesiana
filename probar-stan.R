library(cmdstanr)
library(posterior)
library(bayesplot)
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)

fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 2,
  refresh = 500
)
fit$summary()
mcmc_hist(fit$draws("theta"))

# Checar stanfit
stanfit <- rstan::read_stan_csv(fit$output_files())
stanfit

# Checar shinystan
#  shinystan::launch_shinystan(stanfit)
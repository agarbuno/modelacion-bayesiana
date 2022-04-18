## Setup --------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Librerias para modelacion bayesiana
library(cmdstanr)
library(posterior)
library(bayesplot)

## Modelo conjugado ------------------

set.seed(108791)
sim <- list(mu = rnorm(1))
sim |> as.data.frame()

data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
data

params <- tibble(mu = rnorm(4, data$y/3, sd = sqrt(2/3)))
params |> as.data.frame()

params |>
  mutate(indicadora = ifelse(mu < sim$mu, 1, 0)) |>
  as.data.frame()

experimento <- function(id){
  sim <- list(mu = rnorm(1))
  data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
  mu <- rnorm(4, data$y/3, sd = sqrt(2/3))
  sum(mu < sim$mu)
}

resultados <- tibble(id = 1:100) |>
   mutate(rank = map_dbl(id, experimento))

resultados |>
  ggplot(aes(rank)) +
  geom_hline(yintercept = 20, lty = 2) +
  annotate("rect",
           ymin = qbinom(.95, 100, .2),
           ymax = qbinom(.05, 100, .2),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1, color = "white") + sin_lineas +
  scale_y_continuous(breaks=NULL) + ylab("") + xlab("Estadístico de orden")

n_ranks <- 20
n_reps  <- 5000

experimento <- function(id){
  sim <- list(mu = rnorm(1))
  data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
  mu <- rnorm(n_ranks - 1, data$y/3, sd = sqrt(2/3))
  sum(mu < sim$mu)
}

resultados <- tibble(id = 1:n_reps) |>
  mutate(rank = map_dbl(id, experimento))

res.unif <- resultados

resultados |>
  ggplot(aes(rank)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2) +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1, color = "white") + sin_lineas +
  scale_y_continuous(breaks=NULL) + ylab("") + xlab("Estadístico de orden")

library(pammtools)
g1 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         cdf.lo = cdf - 1/n_ranks + rep(qbinom(.025, n_reps, 1/n_ranks), n_ranks)/n_reps,
         cdf.hi = cdf - 1/n_ranks + rep(qbinom(.975, n_reps, 1/n_ranks), n_ranks)/n_reps) |>
  ggplot(aes(x = rank)) +
  geom_step(aes(y = cdf), lty = 2, color = "gray30") +
  geom_stepribbon(aes(ymin = cdf.lo, ymax = cdf.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = ecdf)) +
  sin_lineas +
  ylab("Función de acumulación") + xlab("Estadístico de orden")

g2 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps),
         diff.hi  = + 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

g1 + g2

n_ranks <- 20
n_reps  <- 5000

experimento <- function(id){
  sim <- list(mu = rnorm(1))
  data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
  mu <- rnorm(n_ranks - 1, data$y/3, sd = 2/3)
  sum(mu < sim$mu)
}

resultados <- tibble(id = 1:n_reps) |>
  mutate(rank = map_dbl(id, experimento))

g0 <- resultados |>
  ggplot(aes(rank)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2) +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1, color = "white") + sin_lineas +
  scale_y_continuous(breaks=NULL) + ylab("") + xlab("Estadístico de orden")

g1 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         cdf.lo = cdf - 1/n_ranks + rep(qbinom(.025, n_reps, 1/n_ranks), n_ranks)/n_reps,
         cdf.hi = cdf - 1/n_ranks + rep(qbinom(.975, n_reps, 1/n_ranks), n_ranks)/n_reps) |>
  ggplot(aes(x = rank)) +
  geom_step(aes(y = cdf), lty = 2, color = "gray30") +
  geom_stepribbon(aes(ymin = cdf.lo, ymax = cdf.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = ecdf)) +
  sin_lineas +
  ylab("Función de acumulación") + xlab("Estadístico de orden")

g2 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps),
         diff.hi  = + 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

res.sub <- resultados
g0 + g1 + g2

n_ranks <- 20
n_reps  <- 5000

experimento <- function(id){
  sim <- list(mu = rnorm(1))
  data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
  mu <- rnorm(n_ranks - 1, data$y/3, sd = sqrt(4/3))
  sum(mu < sim$mu)
}

resultados <- tibble(id = 1:n_reps) |>
  mutate(rank = map_dbl(id, experimento))
res.over <- resultados

g0 <- resultados |>
  ggplot(aes(rank)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2) +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1, color = "white") + sin_lineas +
  scale_y_continuous(breaks=NULL) + ylab("") + xlab("Estadístico de orden")

g1 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         cdf.lo = cdf - 1/n_ranks + rep(qbinom(.025, n_reps, 1/n_ranks), n_ranks)/n_reps,
         cdf.hi = cdf - 1/n_ranks + rep(qbinom(.975, n_reps, 1/n_ranks), n_ranks)/n_reps) |>
  ggplot(aes(x = rank)) +
  geom_step(aes(y = cdf), lty = 2, color = "gray30") +
  geom_stepribbon(aes(ymin = cdf.lo, ymax = cdf.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = ecdf)) +
  sin_lineas +
  ylab("Función de acumulación") + xlab("Estadístico de orden")

g2 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps),
         diff.hi  = + 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

g0 + g1 + g2

n_ranks <- 20
n_reps  <- 5000

experimento <- function(id){
  sim <- list(mu = rnorm(1))
  data <- list(y = rnorm(1, sim$mu, sd = sqrt(2)))
  mu <- rnorm(n_ranks - 1, (1 + data$y)/3, sd = sqrt(2/3))
  sum(mu < sim$mu)
}

resultados <- tibble(id = 1:n_reps) |>
  mutate(rank = map_dbl(id, experimento))
res.bias   <- resultados

g0 <- resultados |>
  ggplot(aes(rank)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2) +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(binwidth = 1, color = "white") + sin_lineas +
  scale_y_continuous(breaks=NULL) + ylab("") + xlab("Estadístico de orden")

g1 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         cdf.lo = cdf - 1/n_ranks + rep(qbinom(.025, n_reps, 1/n_ranks), n_ranks)/n_reps,
         cdf.hi = cdf - 1/n_ranks + rep(qbinom(.975, n_reps, 1/n_ranks), n_ranks)/n_reps) |>
  ggplot(aes(x = rank)) +
  geom_step(aes(y = cdf), lty = 2, color = "gray30") +
  geom_stepribbon(aes(ymin = cdf.lo, ymax = cdf.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = ecdf)) +
  sin_lineas +
  ylab("Función de acumulación") + xlab("Estadístico de orden")

g2 <- resultados |>
  group_by(rank) |>
  tally() |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps),
         diff.hi  = + 2 * sqrt(rank/n_ranks * (1 - rank/n_ranks)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

g0 + g1 + g2

## Caso: escuelas ------------------------------
modelos_files <- "modelos/compilados/calibracion"
ruta <- file.path("modelos/calibracion/escuelas.stan")
modelo.bp <- cmdstan_model(ruta, dir = modelos_files)

n_reps <- 500
n_ranks <- 20

crea_muestras <- function(id, modelo){
  muestras <- modelo$sample(chains = 1,
                            iter_warmup   = 5000,
                            iter_sampling = 990,
                            thin = 10,
                            refresh = 0,
                            seed = id)
  muestras$draws(format = 'df') |>
    as_tibble() |>
    select(mu_lt_sim, tau_lt_sim, theta1_lt_sim) |>
    summarise(rank_mu = sum(mu_lt_sim),
              rank_tau = sum(tau_lt_sim),
              rank_theta1 = sum(theta1_lt_sim))
}
## Cuidado en correr (paciencia)
resultados.escuelas <- tibble(id = 1:n_reps) |>
  mutate(results = map(id, crea_muestras, modelo.bp))

resultados.escuelas |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  ggplot(aes(x = value)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2, color = 'black') +
  annotate("rect",
            ymin = qbinom(.975, n_reps, 1/n_ranks),
            ymax = qbinom(.025, n_reps, 1/n_ranks),
            xmin = -Inf, xmax = Inf,
            alpha = .4, fill = "gray") + 
  geom_histogram(bins = n_ranks, color = "white") +
  facet_wrap(~name) +
  sin_lineas

resultados.escuelas |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  mutate(bins = cut(value, breaks = seq(0,100, length.out= 21))) |>
  group_by(name, bins) |>
  tally() |>
  filter(!is.na(bins)) |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         rank = seq(2.5, 100, 5),
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/100 * (1 - rank/100)/n_reps),
         diff.hi  = + 2 * sqrt(rank/100 * (1 - rank/100)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas + facet_wrap(~name) +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

## Caso: mezclas poisson -------------------------------
modelos_files <- "modelos/compilados/calibracion"
ruta <- file.path("modelos/calibracion/poisson-mix.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

generate_poisson_mix <- function(N){
  ## Generamos parametros simulados
  mu1 <- rnorm(1, 3, 1)
  mu2 <- rnorm(1, 3, 1)
  omega <- runif(1)
  ## Generamos datos sinteticos
  y <- numeric(N)
  for(n in 1:N){
    if(runif(1) < omega){
      y[n] <- rpois(1, exp(mu1))
    } else {
      y[n] <- rpois(1, exp(mu2))
    }
  }
  ## Regresamos en lista
  sim <- within(list(), {
                mu <- c(mu1, mu2)
                omega <- omega
  })
  obs <- list(N = N, y = y)
  list(sim = sim, obs = obs)
}

replicate_experiment <- function(id, modelo){
  data <- generate_poisson_mix(50)
  posterior <- modelo$sample(data$obs, chains = 1, refresh = 1000,
                             iter_sampling = 990, thin = 10)

  posterior$draws(format = "df") |>
    as_tibble() |>
    mutate(
      mu1_bool = mu1 < data$sim$mu[1],
      mu2_bool = mu2 < data$sim$mu[2],
      omega_bool = omega < data$sim$omega) |>
    summarise(
      mu1_rank = sum(mu1_bool),
      mu2_rank = sum(mu2_bool),
      omega_rank = sum(omega_bool), 
      )
}
simulaciones <- tibble(id = 1:500) |>
  mutate(results = map(id, replicate_experiment, modelo))

n_reps <- 500

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  ggplot(aes(x = value)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2, color = 'black') +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(bins = n_ranks, color = "white") +
  facet_wrap(~name) +
  sin_lineas

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  mutate(bins = cut(value, breaks = seq(0,100, length.out= 21))) |>
  group_by(name, bins) |>
  tally() |>
  filter(!is.na(bins)) |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         rank = seq(2.5, 100, 5),
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/100 * (1 - rank/100)/n_reps),
         diff.hi  = + 2 * sqrt(rank/100 * (1 - rank/100)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas + facet_wrap(~name) +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

data <- generate_poisson_mix(50)
posterior <- modelo$sample(data$obs, chains = 4,
                           refresh = 1000,
                           iter_sampling = 4000,
                           seed = 108729)
mcmc_pairs(posterior$draws(),
           regex_pars = "mu",
           pars = c("omega"), 
           off_diag_fun = "hex")

## Caso: mezclas poisson implementacion ----------------------
modelos_files <- "modelos/compilados/calibracion"
ruta <- file.path("modelos/calibracion/poisson-mix-full.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

simulaciones <- tibble(id = 1:500) |>
    mutate(results = map(id, replicate_experiment, modelo))

n_reps <- 500

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  ggplot(aes(x = value)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2, color = 'black') +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(bins = n_ranks, color = "white") +
  facet_wrap(~name) +
  sin_lineas

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  mutate(bins = cut(value, breaks = seq(0,100, length.out= 21))) |>
  group_by(name, bins) |>
  tally() |>
  filter(!is.na(bins)) |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         rank = seq(2.5, 100, 5),
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/100 * (1 - rank/100)/n_reps),
         diff.hi  = + 2 * sqrt(rank/100 * (1 - rank/100)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas + facet_wrap(~name) +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

set.seed(108795)
data <- generate_poisson_mix(50)
posterior <- modelo$sample(data$obs, chains = 4,
                           refresh = 1000,
                           iter_warmup   = 2000,
                           iter_sampling = 2000,
                           seed = 108729)
mcmc_pairs(posterior$draws(),
           regex_pars = "mu",
           pars = c("omega"), 
           off_diag_fun = "hex")

## Caso: mezclas poisson ordenadas ----------------------
modelos_files <- "modelos/compilados/calibracion"
ruta <- file.path("modelos/calibracion/poisson-mix-ordered.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

generate_poisson_mix_ordered <- function(N){
  ## Generamos parametros simulados
  mu <- sort(rnorm(2, 3, 1))
  omega <- runif(1)
  ## Generamos datos sinteticos
  y <- numeric(N)
  for(n in 1:N){
    if(runif(1) < omega){
      y[n] <- rpois(1, exp(mu[1]))
    } else {
      y[n] <- rpois(1, exp(mu[2]))
    }
  }
  ## Regresamos en lista
  sim <- within(list(), {
                mu <- mu
                omega <- omega
  })
  obs <- list(N = N, y = y)
  list(sim = sim, obs = obs)
}

replicate_experiment_ordered <- function(id, modelo){
  data <- generate_poisson_mix_ordered(50)
  posterior <- modelo$sample(data$obs, chains = 1, refresh = 1000,
                             iter_sampling = 990, thin = 10)

  posterior$draws(format = "df") |>
    as_tibble() |>
    mutate(
      mu1_bool = `mu[1]` < data$sim$mu[1],
      mu2_bool = `mu[2]` < data$sim$mu[2],
      omega_bool = omega < data$sim$omega) |>
    summarise(
      mu1_rank = sum(mu1_bool),
      mu2_rank = sum(mu2_bool),
      omega_rank = sum(omega_bool), 
      )
}

simulaciones <- tibble(id = 1:500) |>
    mutate(results = map(id, replicate_experiment_ordered, modelo))

n_reps <- 500

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  ggplot(aes(x = value)) +
  geom_hline(yintercept = n_reps/n_ranks, lty = 2, color = 'black') +
  annotate("rect",
           ymin = qbinom(.975, n_reps, 1/n_ranks),
           ymax = qbinom(.025, n_reps, 1/n_ranks),
           xmin = -Inf, xmax = Inf,
           alpha = .4, fill = "gray") + 
  geom_histogram(bins = n_ranks, color = "white") +
  facet_wrap(~name) +
  sin_lineas

simulaciones |>
  unnest(results) |>
  pivot_longer(cols = 2:4) |>
  mutate(bins = cut(value, breaks = seq(0,100, length.out= 21))) |>
  group_by(name, bins) |>
  tally() |>
  filter(!is.na(bins)) |>
  mutate(ecdf = cumsum(n)/sum(n),
         cdf  = 1:n_ranks/n_ranks,
         rank = seq(2.5, 100, 5),
         diff.cdf = ecdf - cdf,
         diff.lo  = - 2 * sqrt(rank/100 * (1 - rank/100)/n_reps),
         diff.hi  = + 2 * sqrt(rank/100 * (1 - rank/100)/n_reps), 
         ) |>
  ggplot(aes(x = rank)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray30") + 
  geom_stepribbon(aes(ymin = diff.lo, ymax = diff.hi), fill = "grey70", alpha = .3) +
  geom_step(aes(y = diff.cdf)) +
  sin_lineas + facet_wrap(~name) +
  ylab("Diferencia de acumulación") + xlab("Estadístico de orden")

set.seed(108795)
data <- generate_poisson_mix(50)
posterior <- modelo$sample(data$obs, chains = 4,
                           refresh = 1000,
                           iter_warmup   = 2000,
                           iter_sampling = 2000,
                           seed = 108729)
mcmc_pairs(posterior$draws(),
           regex_pars = "mu",
           pars = c("omega"), 
           off_diag_fun = "hex")

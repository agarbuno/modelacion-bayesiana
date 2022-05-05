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

modelos_files <- "modelos/compilados/bandits"
ruta <- file.path("modelos/bandits/tragamonedas-ab.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data.list <- list(K = 3, y = c(2, 9, 32), N = c(2, 10, 40))
posterior <- modelo$sample(data = data.list, refresh = 1000)

g1 <- bayesplot::mcmc_intervals(posterior$draws(), regex_pars = "theta") +
  sin_lineas
g2 <- bayesplot::mcmc_hist(posterior$draws(), regex_pars = "theta") +
  sin_lineas + xlim(0,1)
g2/ g1 + patchwork::plot_layout(heights = c(3, 2))

posterior$draws(variables = "mejor_ix", format = "df") |>
  as_tibble() |>
  pivot_longer(cols = 1:3) |>
  group_by(name) |>
  summarise(gana = sum(value)) |>
  ggplot(aes(x = 1:3, y = gana)) +
  geom_col() +
  xlab("Opción") + ylab("Conteos") + sin_lineas

ruta <- file.path("modelos/bandits/tragamonedas-bernoulli.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

ruta <- file.path("modelos/bandits/tragamonedas-conjugado.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

## Declaramos el problema
K <- 2
theta <- c(0.5, 0.4)
N <- 1000

## Inicializamos
p_best <- matrix(0, N, K)
r_hat <- matrix(0, N, K)
y <- array(0.0, 0)
z <- array(0.0, 0)
prefix <- function(y, n) array(y, dim = n - 1)

## Hacemos el aprendizaje secuencial
for (n in 1:N) {
  data <- list(K = K, N = n - 1, y = prefix(y, n), z = prefix(z, n))
  posterior <- modelo$sample(data, fixed_param = TRUE,
                             chains = 1, iter_sampling = 1000, refresh = 0)
  p_best[n, ] <- posterior$summary(variables = "mejor_ix")$mean
  r_hat[n, ] <- posterior$summary(variables = "theta")$rhat
  z[n] <- sample(K, 1, replace = TRUE, p_best[n, ])
  y[n] <- rbinom(1, 1, theta[z[n]])
}

g1 <- tibble(rhat = r_hat[,1]) |>
  ggplot(aes(rhat)) +
  geom_histogram() + sin_lineas


g2 <- tibble(p1 = p_best[,1],
       Turno = 1:N) |>
  ggplot(aes(Turno, p1)) +
  geom_line() + sin_lineas +
  scale_x_log10() +
  ylab("P[θ1 > θ2]")


g1 + g2

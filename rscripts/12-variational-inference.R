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

modelos_files <- "modelos/compilados/variacional"
ruta <- file.path("modelos/variacional/bernoulli.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
posterior <- modelo$sample(stan_data, seed = 123, chains = 2, refresh = 1000)

posterior$summary() |> as.data.frame()

bayesplot::mcmc_hist(posterior$draws("theta")) + sin_lineas

posterior.advi <- modelo$variational(stan_data, seed = 123, output_samples = 2000)

posterior.advi$summary() |>
as.data.frame()

bayesplot::mcmc_hist(posterior.advi$draws("theta")) + sin_lineas

posterior$draws("theta", format = "df") |>
  rbind(posterior.advi$draws("theta", format = "df")) |>
  as_tibble() |>
  mutate(método = rep(c("mcmc", "advi"), each = 2000)) |>
  ggplot(aes(theta, group = método, fill = método)) +
  geom_histogram(position = "identity", alpha = .6) +
  sin_lineas

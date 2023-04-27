## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)
## Problemas con mi consola en Emacs
options(pillar.subtle = FALSE)
options(rlang_backtrace_on_error = "none")
options(crayon.enabled = FALSE)

## Para el tema de ggplot
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Librerias para modelacion bayesiana
library(cmdstanr)
library(posterior)
library(bayesplot)

g1 <- tibble(x = seq(-5, 5, length.out = 200)) |>
  mutate(y = dnorm(x, 0, 2)) |>
  ggplot(aes(x, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), color = "gray70", alpha = .3) + 
  geom_vline(xintercept = -2, lty = 2, color = 'red') +
  sin_lineas + sin_ejes +
  ylab("Densidad") + xlab("") +
  ggtitle("erróneo / incierto")

g2 <- tibble(x = seq(-5, 5, length.out = 200)) |>
  mutate(y = dnorm(x, 0,.5)) |>
  ggplot(aes(x, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), color = "gray70", alpha = .3) + 
  geom_vline(xintercept = -2, lty = 2, color = 'red') +
  sin_lineas + sin_ejes +
  ylab("Densidad") + xlab("") +
  ggtitle("erróneo / confiado")

g3 <- tibble(x = seq(-5, 5, length.out = 200)) |>
  mutate(y = dnorm(x, -2, 2)) |>
  ggplot(aes(x, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), color = "gray70", alpha = .3) + 
  geom_vline(xintercept = -2, lty = 2, color = 'red') +
  sin_lineas + sin_ejes +
  ylab("Densidad") + xlab("") +
  ggtitle("acertado / incierto")

g4 <- tibble(x = seq(-5, 5, length.out = 200)) |>
  mutate(y = dnorm(x, -2, .5)) |>
  ggplot(aes(x, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), color = "gray70", alpha = .3) + 
  geom_vline(xintercept = -2, lty = 2, color = 'red') +
  sin_lineas + sin_ejes +
  ylab("Densidad") + xlab("") +
  ggtitle("acertado / confiado")

(g1 + g2) / (g3 + g4)

g1 <- tibble(q = seq(0.01, .99, length.out = 150)) |>
  mutate(kl = map_dbl(q, function(x){
    0.3 * (log(.3) - log(x)) + 0.7 * (log(.7) - log(1-x))
  })) |>
  ggplot(aes(q, kl)) +
  geom_line() +
  geom_vline(xintercept = .3, lty = 2, color = 'red') +
  ylab("Divergencia KL") + xlab(expression(q[1])) +
      sin_lineas

g2 <- tibble(q = seq(0.01, .99, length.out = 150)) |>
  mutate(kl = map_dbl(q, function(x){
    0.3 * (log(.3) - log(x)) + 0.7 * (log(.7) - log(1-x))
      })) |>
  ggplot(aes(q, kl)) +
  geom_line() +
  geom_vline(xintercept = .3, lty = 2, color = 'red') +
      scale_y_log10() +
  ylab("log-Divergencia KL") + xlab(expression(q[1])) +
  sin_lineas

g1 + g2

## Caso: escuelas ------------------------------------------------------------
data <- tibble( id = factor(seq(1, 8)), 
                y = c(28, 8, -3, 7, -1, 1, 18, 12), 
                sigma = c(15, 10, 16, 11, 9, 11, 10, 18))

data.list <- c(data, J = 8)

modelos_files <- "modelos/compilados/comparacion"
ruta <- file.path("modelos/comparacion/escuelas-indep.stan")
modelo.indep <- cmdstan_model(ruta, dir = modelos_files)

library(loo)
posterior.indep <- modelo.indep$sample(data.list, refresh = 500)
stanfit <- rstan::read_stan_csv(posterior.indep$output_files())
log_lik <- extract_log_lik(stanfit, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 2)

waic(log_lik, r_eff = r_eff)

loo(log_lik, r_eff = r_eff)

calcula_metricas <- function(posterior){
  log_lik <- posterior$draws(variables = "log_lik", format = "array")
  r_eff <- relative_eff(exp(log_lik), cores = 2) 
  within(list(), {
    loo  <- loo(log_lik, r_eff = r_eff)
    waic <- waic(log_lik, r_eff = r_eff)
  })
}

ruta <- file.path("modelos/comparacion/escuelas-agrup.stan")
modelo.agrup <- cmdstan_model(ruta, dir = modelos_files)
posterior.agrup <- modelo.agrup$sample(data.list, refresh = 500)

ruta <- file.path("modelos/comparacion/escuelas-jerar.stan")
modelo.jerar <- cmdstan_model(ruta, dir = modelos_files)
posterior.jerar <- modelo.jerar$sample(data.list, refresh = 500)

indep.metricas <- calcula_metricas(posterior.indep)
agrup.metricas <- calcula_metricas(posterior.agrup)
jerar.metricas <- calcula_metricas(posterior.jerar)

waic.diferencias <- loo_compare(list(
  indep = indep.metricas$waic,
  agrup = agrup.metricas$waic,
  jerar = jerar.metricas$waic
))
print(waic.diferencias, simplify = FALSE)

loo.diferencias <- loo_compare(list(
  indep = indep.metricas$loo,
  agrup = agrup.metricas$loo,
  jerar = jerar.metricas$loo
))
print(loo.diferencias, simplify = FALSE)

waic.diferencias |>
  as_tibble() |>
  mutate(modelo = rownames(waic.diferencias)) |>
  ggplot(aes(waic, modelo)) +
  geom_vline(aes(xintercept = min(waic)), lty = 2) + 
  geom_linerange(aes(xmax = waic + 2 * se_waic,
                     xmin = waic - 2 * se_waic)) +
  geom_linerange(aes(xmax = waic + 1 * se_waic,
                     xmin = waic - 1 * se_waic), size = 2) + 
  geom_point(color = "red", size = 3) +
  sin_lineas

loo.diferencias |>
  as_tibble() |>
  mutate(modelo = rownames(loo.diferencias)) |>
  ggplot(aes(looic, modelo)) +
  geom_vline(aes(xintercept = min(looic)), lty = 2) + 
  geom_linerange(aes(xmax = looic + 2 * se_looic,
                     xmin = looic - 2 * se_looic)) +
  geom_linerange(aes(xmax = looic + 1 * se_looic,
                     xmin = looic - 1 * se_looic), size = 2) + 
  geom_point(color = "red", size = 3) +
  sin_lineas

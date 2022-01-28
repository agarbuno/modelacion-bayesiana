## Setup ----------------------------------------
library(tidyverse)
library(patchwork)
set.seed(108727)
## Cambia el default del tamaño de fuente 
theme_set(theme_grey(base_size = 18))

## Cambia el número de decimales para mostrar
options(digits = 2)

## Ejemplo peliculas

## Diferentes previas, diferentes posteriores --------------------

modelo_beta <- function(params, n = 5000){
  rbeta(n, params$alpha, params$beta)
}

escenarios <-
  tibble(analista = fct_inorder(c("Ignorante", "Indiferente",
                                  "Feminista", "Ingenuo")),
         alpha = c(1, .5, 5, 14),
         beta  = c(1, .5, 11, 1)) |>
  nest(params.previa = c(alpha, beta)) |>
  mutate(muestras.previa = map(params.previa, modelo_beta))

escenarios |>
  unnest(muestras.previa) |>
  ggplot(aes(muestras.previa)) +
  geom_histogram(binwidth = .05) +
  facet_wrap(.~analista, scales = "free_y", ncol = 4) +
  xlab("Proporción de películas")

escenarios |>
  unnest(muestras.previa) |>
  mutate(peliculas = map_dbl(muestras.previa,
                         function(theta) rbinom(1, 33, theta))) |>
  ggplot(aes(peliculas)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(.~analista, scales = "free_y", ncol = 4)

library(bayesrules)
set.seed(108727)
data <- bechdel |>
  sample_n(20)

data <- data |>
  group_by(binary) |>
  tally() |>
  pivot_wider(names_from = binary,
              values_from = n) |>
  as.data.frame()

update_rule <- function(params){
  tibble(alpha = params$alpha + data$PASS,
         beta  = params$beta  + data$FAIL)
}
escenarios <- escenarios |>
  mutate(params.posterior = map(params.previa, update_rule),
         muestras.posterior = map(params.posterior, modelo_beta))

escenarios |>
  pivot_longer(cols = c(muestras.previa, muestras.posterior)) |>
  unnest(value) |>
  ggplot(aes(value, group = name, fill = name)) +
  geom_histogram(position = "identity", alpha = .7) +
  facet_wrap(.~analista, ncol = 4, scales = "free_y") +
  geom_vline(xintercept = data$PASS / 20, lty = 2) +
  xlab("Proporción de películas")

escenarios |>
 unnest(muestras.posterior) |>
    mutate(peliculas = map_dbl(muestras.posterior,
                           function(theta) rbinom(1, 33, theta))) |>
    ggplot(aes(peliculas)) +
    geom_histogram(binwidth = 3) +
    facet_wrap(.~analista, scales = "free_y", ncol = 4)

## Diferentes datos, diferentes posteriores -------------------

extrae_datos <- function(n){
  bechdel |>
    sample_n(n) |>
    group_by(binary) |>
    tally() |>
    pivot_wider(names_from = binary,
                values_from = n) |>
    as.data.frame()
}

update_rule <- function(data){
    tibble(alpha = params.fem$alpha + data$PASS,
           beta  = params.fem$beta  + data$FAIL)
}

params.fem <- list(alpha = 5, beta = 11)

escenarios <- tibble(id = seq(1, 4),
       n = c(5, 20, 100, 500),
       datos = map(n, extrae_datos))

escenarios <- escenarios |>
  mutate(params.posterior = map(datos, update_rule),
         muestras.posterior = map(params.posterior, modelo_beta),
         muestras.previa    = list(modelo_beta(params.fem)))

escenarios |>
   pivot_longer(cols = c(muestras.previa, muestras.posterior)) |>
   unnest(value) |>
   ggplot(aes(value, group = name, fill = name)) +
   geom_histogram(aes(y = ..density..), position = "identity", alpha = .7) +
   facet_wrap(.~n, ncol = 4) +
  xlab("Proporción de películas")

bechdel |>
  group_by(year, binary) |>
  tally() |>
  pivot_wider(values_from = n,
              names_from = binary,
              values_fill = 0) |>
  mutate(rate = PASS/(PASS+FAIL)) |>
  ggplot(aes(year, rate)) +
  geom_line() + geom_point()

## Analisis secuencial ------------------------------
library(ggridges)

tibble(period = "previa", FAIL = 0, PASS = 0) |>
  rbind(bechdel |>
        mutate(period = cut(year, breaks = 5)) |>
        group_by(period) |>
        sample_frac(.3) |>
        ungroup() |>
        group_by(period, binary) |>
        tally() |>
        ungroup() |>
        pivot_wider(values_from = n,
                    names_from = binary,
                    values_fill = 0)) |>
  summarise(period = fct_inorder(period),
            pass = cumsum(PASS),
            fail = cumsum(FAIL),
            rate = pass/(pass + fail),
            alpha = 5 + pass,
            beta  = 11 + fail) |>
  nest(params = c(alpha, beta)) |>
  mutate(muestras = map(params, modelo_beta)) |>
  unnest(muestras, params) |>
  ggplot(aes(muestras, period)) +
  geom_density_ridges(stat = "binline", bins = 40) +
  geom_point(aes(x = pass/(pass + fail), y = period), fill = 'lightblue', shape = 23, size = 5) +
  ## geom_point(aes(x = alpha/(alpha + beta), y = period), fill = 'red', shape = 23, size = 5) + 
  xlim(0,1) + xlab("Tasa de éxito")

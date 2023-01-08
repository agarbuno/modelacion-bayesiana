## Setup --------------------------------------------
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

genera_circulo <- function(n = 10){
  tibble(angulo = seq(0, 2*pi, length.out = n),
         x = sin(angulo), y = cos(angulo))
}

tibble(n = 2**c(2.25, 3, 4, 8)) |>
  mutate(datos = map(n, genera_circulo)) |>
  unnest(datos) |>
  ggplot(aes(x, y)) + 
  geom_path(aes(group = n, lty = factor(n))) +
  coord_equal() + xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + sin_leyenda + sin_ejes

## Ejemplo regresion ---------------------------------------------------------

data(mpg)
data <- mpg |> as_tibble()
data |> print(n = 5)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy)) +
  sin_lineas

## Modelo de regresion -------------------------------------------------------

glm(hwy ~ cty, data, family = gaussian()) |>
  summary()

## Modelo de regresion (bayesiano) -------------------------------------------

library(rstanarm)
stan_glm(hwy ~ cty, data = data, refresh = 0) |>
  summary()

## Diferentes previas, diferentes posteriores --------------------------------

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
  xlab("Proporción de películas") + sin_lineas

escenarios |>
  unnest(muestras.previa) |>
  mutate(peliculas = map_dbl(muestras.previa,
                         function(theta) rbinom(1, 33, theta))) |>
  ggplot(aes(peliculas)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(.~analista, scales = "free_y", ncol = 4) + sin_lineas

library(bayesrules)
set.seed(108727)
data <- bechdel |>
  sample_n(20)

data <- data |>
  group_by(binary) |>
  tally() |>
  pivot_wider(names_from = binary,
              values_from = n)

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
  xlab("Proporción de películas") + sin_lineas

escenarios |>
 unnest(muestras.posterior) |>
    mutate(peliculas = map_dbl(muestras.posterior,
                           function(theta) rbinom(1, 33, theta))) |>
    ggplot(aes(peliculas)) +
    geom_histogram(binwidth = 3) +
    facet_wrap(.~analista, scales = "free_y", ncol = 4) + sin_lineas

## Diferentes datos, diferentes posteriores -------------------

extrae_datos <- function(n){
  bechdel |>
    sample_n(n) |>
    group_by(binary) |>
    tally() |>
    pivot_wider(names_from = binary,
                values_from = n)
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
  xlab("Proporción de películas") + sin_lineas

## La posterior de hoy es la previa de mañana --------------------------------

bechdel |>
  group_by(year, binary) |>
  tally() |>
  pivot_wider(values_from = n,
              names_from = binary,
              values_fill = 0) |>
  mutate(rate = PASS/(PASS+FAIL)) |>
  ggplot(aes(year, rate)) +
  geom_line() + geom_point() + sin_lineas

## Analisis secuencial -------------------------------------------------------
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
  xlim(0,1) + xlab("Tasa de éxito") + sin_lineas

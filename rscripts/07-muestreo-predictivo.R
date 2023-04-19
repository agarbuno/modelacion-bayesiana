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

bayesplot::bayesplot_theme_set(bayesplot::theme_default())
options(bayesplot.base_size = 25)

## Cantantes: Predictiva previa  ---------------------------------------------

previa.params  <- within(list(),
{
  mu0 <- 175
  n0  <- 5
  a0  <- 3
  b0  <- 147
})

modelos_files <- "modelos/compilados/predictivos"
ruta <- file.path("modelos/predictivos/cantantes-previa.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

replica <- function(id){
  previa <- modelo$sample(previa.params,
                          fixed_param = TRUE,
                          refresh = 0, chains = 1,
                          show_messages = FALSE)
  list(mean = mean(previa$draws(format = "df")$y_tilde),
       sd   = sd(previa$draws(format = "df")$y_tilde),
       min  = min(previa$draws(format = "df")$y_tilde),
       max  = max(previa$draws(format = "df")$y_tilde))
  }

resultados <- tibble(id = 1:200) |>
  mutate(results = map_df(id, replica))

g1 <- resultados |>
  unnest(results) |>
  ggplot(aes()) +
  annotate(geom = "rect", ymin = 0, ymax = 100, xmin = 170, xmax = 173, alpha = .3) +
  annotate(geom = "rect", ymin = 0, ymax = 100, xmin = 177, xmax = 180, alpha = .3) +
  sin_lineas +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab(expression(bar(y)[n])) +
  ggtitle("Región de resultados extremos")

g2 <- resultados |>
  unnest(results) |>
  ggplot(aes(mean)) +
  geom_histogram(bins = 30) +
  annotate(geom = "rect", ymin = 0, ymax = Inf, xmin = 170, xmax = 173, alpha = .3) +
  annotate(geom = "rect", ymin = 0, ymax = Inf, xmin = 177, xmax = 180, alpha = .3) +
  sin_lineas + ylab("") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab(expression(bar(y)[n])) +
  ggtitle("Réplicas de promedios")

g1 + g2

resultados |>
  unnest(results) |>
  pivot_longer(cols = mean:max, names_to = 'estadistico') |>
  ggplot(aes(value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~estadistico, scales = "free") +
  sin_lineas

## Predictiva previa soccer ---------------------------
modelos_files <- "modelos/compilados/predictivos"
ruta <- file.path("modelos/predictivos/soccer-previa-predictivo.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

params.previa <- within(list(),{
  J <- 18
  epsilon <- c(0.5, 0.00001)  
})
pprevia <- modelo$sample(params.previa, fixed_param = TRUE,
                         refresh = 0, seed = 10872791)

pprevia$draws(variables = "y", format = "df") |>
  pull(`y[1,2]`) |>
  head(20) |>
  matrix(nrow = 2)

pprevia$draws(variables = "y", format = "df") |>
  mutate(goles = `y[1,2]`) |>
  ggplot(aes(goles)) +
  geom_histogram() +
  xlab("Diferencia de goles") + sin_lineas

## Cantantes: modelo posterior -----------------------------------------------

## Leemos los datos
set.seed(3413)
cantantes <- lattice::singer |>
  mutate(estatura_cm = round(2.54 * height)) |>
  filter(str_detect(voice.part, "Tenor")) |>
  sample_n(42)

## Preparamos el modelo
modelos_files <- "modelos/compilados/predictivos"
ruta <- file.path("modelos/predictivos/cantantes-posterior.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data.list <- within(list(), {
  N <- 42
  y <- cantantes$estatura_cm
})
posterior <- modelo$sample(append(previa.params, data.list),
                           refresh = 0, seed = 108727)

set_cmdstan_path("~/.cmdstan/cmdstan")
modelos_files <- "modelos/compilados/predictivos"
ruta <- file.path("modelos/predictivos/cantantes-posterior-completo.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

posterior <- modelo$sample(append(previa.params, data.list), refresh = 0)
y_rep <- posterior$draws(variables = "y_tilde", format = "matrix")
ppc_hist(cantantes$estatura_cm, y_rep[1:15,], binwidth = 5) + sin_lineas

ppc_dens_overlay(cantantes$estatura_cm, y_rep[1:100,], alpha = .8) + sin_lineas

ppc_boxplot(cantantes$estatura_cm, y_rep[1:8,]) + sin_lineas

ppc_intervals(cantantes$estatura_cm, y_rep, size = 1.5) + sin_lineas

ppc_ribbon(cantantes$estatura_cm, y_rep, y_draw = "points") + sin_lineas

ppc_stat(cantantes$estatura_cm, y_rep, alpha = .8) + sin_lineas

ppc_stat(cantantes$estatura_cm, y_rep, alpha = .8, stat = "sd") + sin_lineas

ppc_stat(cantantes$estatura_cm, y_rep, alpha = .8, stat = function(x) quantile(x, .95)) + sin_lineas

ppc_stat_2d(cantantes$estatura_cm, y_rep, alpha = .8, stat = c("mean", "sd")) + sin_lineas

posterior$draws(
  ## Extraigo lo que me interesa 
            variables = c("mean_y_tilde", "sd_y_tilde"),
            format = "df") |>
  ## Calculo indicadoras I{s(muestras) > s(datos)}  
  mutate(indicadora.mean = mean_y_tilde >= mean(cantantes$estatura_cm),
         indicadora.sd   = sd_y_tilde >= sd(cantantes$estatura_cm)) |>
  ## Calculo estimador Monte Carlo
  summarise(p.value.mean = mean(indicadora.mean),
            p.value.sd   = mean(indicadora.sd))

## Experimentos de velocidad de la luz ---------------------------------------
light <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Newcomb/data/newcomb.txt")

modelos_files <- "modelos/compilados/predictivos"
ruta <- file.path("modelos/predictivos/lightspeed.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

posterior <- modelo$sample(list(N = nrow(light), y = light$y), refresh = 0)

posterior$draws(variables = c("mean_y_tilde", "sd_y_tilde"), format = "df") |>
    mutate(indicadora.mean = mean_y_tilde >= mean(light$y),
           indicadora.sd   = sd_y_tilde >= sd(light$y)) |>
    summarise(p.value.mean = mean(indicadora.mean),
              p.value.sd   = mean(indicadora.sd))

y_rep <- posterior$draws(variables = "y_tilde", format = "matrix")
ppc_stat(light$y, y_rep, alpha = .8) + sin_lineas

ppc_stat(light$y, y_rep, alpha = .8, stat = "sd") + sin_lineas

ppc_hist(light$y, y_rep[1:15,], binwidth = 5) + sin_lineas

ppc_stat(light$y, y_rep, alpha = .8, stat = function(x) {min(x)}) + sin_lineas

ppc_dens_overlay(light$y, y_rep[1:100,], alpha = .8) + sin_lineas

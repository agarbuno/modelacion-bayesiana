## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4, , pillar.width = 75)
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

tweets   <- read_csv("datos/response_times.csv", show_col_types = FALSE)
tweets   <- tweets |>
  mutate(compania = author_id_y,
         fecha = created_at_x,
         anio  = lubridate::year(fecha),
         mes   = lubridate::month(fecha),
         dia   = lubridate::day(fecha))

reclamos <- tweets |>
  select(anio, mes, dia, compania, response_time) |>
  filter(anio == 2017, mes %in% c(10, 11),
         !(compania %in% c("AmericanAir", "Delta", "SouthwestAir"))) |>
  group_by(anio, mes, dia, compania) |>
  summarise(atendidos = n(),
            respuesta = mean(response_time)) |>
  mutate(compania = factor(compania)) |> 
  ungroup()

reclamos |> print(n = 3)

reclamos |>
  ggplot(aes(atendidos)) +
  geom_histogram() + sin_lineas

reclamos |>
  summarise(promedio = mean(atendidos))

modelos_files <- "modelos/compilados/reclamaciones"
ruta <- file.path("modelos/reclamaciones/modelo-poisson.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data_list <- list(N = nrow(reclamos), y = reclamos$atendidos)
previa <- modelo$sample(data = list(N = 0, y = c()), refresh = 0)
posterior <- modelo$sample(data = data_list, refresh = 0)

posterior$summary()

simulaciones <- previa$draws(format = "df") |>
  mutate(dist = "previa") |>
  rbind(posterior$draws(format = "df") |>
        mutate(dist = "posterior"))

simulaciones |>
  ggplot(aes(lambda, fill = dist)) +
  geom_histogram(position = "identity", alpha = .6) +
  ggtitle("Simulaciones de parámetro desconocido") +
sin_lineas

simulaciones |>
  as_tibble() |>
  mutate(y_tilde = map_dbl(lambda, function(x){
    rpois(1, x)
  })) |>
  ggplot(aes(y_tilde, fill = dist)) +
  geom_histogram(position = "identity", alpha = .6) +
  ggtitle("Simulaciones de predictivas") + sin_lineas

g1 <- simulaciones |>
  as_tibble() |>
  mutate(y_tilde = map_dbl(lambda, function(x){
    rpois(1, x)
  })) |>
  ggplot(aes(y_tilde, fill = dist)) +
  geom_histogram(position = "identity", alpha = .6) +
  xlab("atendidos*") +
  ggtitle("Simulaciones de predictivas") + sin_lineas +
  coord_cartesian(xlim = c(0,300))

g2 <- reclamos |>
  ggplot(aes(atendidos)) +
  geom_histogram(position = "identity", alpha = .6) +
  ggtitle("Histograma datos") + sin_lineas + coord_cartesian(xlim = c(0, 300))

g2 + g1

reclamos |>
  summarise(promedio = mean(atendidos),
            varianza = var(atendidos))

modelos_files <- "modelos/compilados/reclamaciones"
ruta <- file.path("modelos/reclamaciones/modelo-negbinom.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data_list <- list(N = nrow(reclamos), y = reclamos$atendidos)
previa <- modelo$sample(data = list(N = 0, y = c()), refresh = 0)
posterior <- modelo$sample(data = data_list, refresh = 500, seed = 108727)

posterior$summary() |> print(n = 5, width = 70)

reclamos |>
  summarise(promedio = mean(atendidos),
            varianza = var(atendidos),
            exceso   = (varianza - promedio)/promedio**2,
            precision = 1/exceso)

modelos_files <- "modelos/compilados/reclamaciones"
ruta <- file.path("modelos/reclamaciones/elicita-gamma.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

solucion <- modelo$sample(iter = 1, iter_warmup = 0,
                          chains = 1, fixed_param = TRUE)
previa.params <- solucion$draws(format = "df")

reclamos |>
  group_by(compania) |>
  summarise(promedio = mean(atendidos),
            varianza = var(atendidos),
            exceso   = (varianza - promedio)/promedio**2,
            precision = 1/exceso)

modelos_files <- "modelos/compilados/reclamaciones"
ruta <- file.path("modelos/reclamaciones/modelo-negbinom-jerarquico.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data_list <- list(N = nrow(reclamos),
                  y = reclamos$atendidos,
                  compania = as.numeric(reclamos$compania),
                  gamma_alpha = previa.params$alpha,
                  gamma_beta  = previa.params$beta)
posterior <- modelo$sample(data = data_list, refresh = 500, seed = 108727)

posterior$summary() |> print(n = 15, width = 75)

mcmc_hist(posterior$draws(),
          regex_pars = "lambda",
          transformations = "exp") + sin_lineas

mcmc_hist(posterior$draws(),
          regex_pars = "phi") + sin_lineas

mcmc_hist(posterior$draws(),
          regex_pars = "tilde") + sin_lineas

## Aplicación: Datos de golf -----------------------------------------------

datos <- read_delim("datos/golf.csv", delim = " ")
datos <- datos |> 
  mutate(x = round(30.48  * x, 0), 
         se = sqrt((y/n)*(1-y/n)/n))

g_datos <- datos |> 
  ggplot(aes(x = x, y = y/n)) + 
    geom_linerange(aes(ymin = y/n - 2 * se, ymax = y/n + 2*se)) + 
    geom_point(colour = "steelblue", alpha = 1.) + 
    ylim(c(0,1)) + xlab("Distancia (cm)") + ylab("Tasa de éxito") + 
    ggtitle("Datos sobre putts en golf profesional") + sin_lineas

g_datos

modelos_files <- "modelos/compilados/golf"
ruta <- file.path("modelos/golf/modelo-logistico.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

ajustar_modelo <- function(modelo, datos, iter_sampling = 1000, iter_warmup = 1000, seed = 2210){ 
  ajuste <- modelo$sample(data = datos, 
                          seed = seed,
                          iter_sampling = iter_sampling, 
                          iter_warmup = iter_sampling,
                          refresh = 0, 
                          show_messages = FALSE)
  ajuste
}

data_list <- c(datos, list("J" = nrow(datos)))
ajuste <- ajustar_modelo(modelo, data_list)

ajuste$summary() |> print(width = 75 )

muestras <- tibble(posterior::as_draws_df(ajuste$draws(c("a", "b"))))
muestras |>
  pivot_longer(cols = c(a, b), names_to = 'parameter') |> 
  mutate(Chain = as.factor(.chain)) |> 
  ggplot(aes(x = .iteration, y = value)) + 
  geom_line(aes(group = .chain, color = Chain)) + 
  facet_wrap(~parameter, ncol = 1, scales = 'free', strip.position="right") + 
  scale_color_viridis_d(option = 'plasma')+ sin_lineas

params_map <- modelo$optimize(data = data_list, seed = 108)

params_map <- params_map$summary() |>
  pivot_wider(values_from = estimate, names_from = variable)
params_map

muestras |> 
  ggplot(aes(x = a, y = b)) + 
  geom_point() + 
  geom_point(data = params_map, aes(x = a, y = b),
             color = 'salmon', shape = 4, stroke = 2) + 
  ggtitle('Muestras de la posterior')+ sin_lineas

logit <- qlogis
invlogit <- plogis

modelo_logistico <- function(a, b){
  x <- seq(0, 1.1 * max(datos$x), length.out = 50)
  tibble(x = x, y = invlogit(a *x + b))
}

curvas_regresion <- muestras |> 
  mutate(curva = map2(a, b, modelo_logistico)) |> 
  select(-a, -b) |> 
  unnest(curva) |> 
  group_by(x) |> 
  summarise(mediana = median(y), 
            q_low = quantile(y, .005), 
            q_hi = quantile(y, .995), 
            .groups = 'drop')

g_logistico <- datos |> 
  ggplot(aes(x = x, y = y/n)) + 
  geom_linerange(aes(ymin = y/n - 2 * se, ymax = y/n + 2*se)) + 
  geom_point(colour = "steelblue", alpha = 1.) + 
  geom_line(data = curvas_regresion, aes(x = x, y = mediana)) +
  geom_ribbon(data = curvas_regresion, aes(x = x, ymin = q_low, ymax = q_hi), 
              alpha = .2, inherit.aes = FALSE) +
  ylim(c(0,1)) + xlab("Distancia (cm)") + ylab("Tasa de éxito") + 
  ggtitle("Regresion logística ajustada")+ sin_lineas

muestras_logistico <- muestras
g_logistico

radios <- tibble(pelota = (1.68/2 * 2.54) |> round(1), 
                  hoyo  = (4.25/2 * 2.54) |> round(1))
radios

tibble(x = seq(10, 1500, 1)) |> 
  mutate(theta = (180 / pi) * atan(3.3 / x)) |> 
ggplot(aes(x, theta)) + geom_line() +
  xlab("Distancia (cm)") +
  ylab(expression(paste("Desviación máxima |", theta,"|"))) +
  scale_y_log10()+ sin_lineas

curva_angulo <- function(sigma){
  x <- seq(0, 650, by = .5)
  R.diff <- radios |> summarise(diff = hoyo - pelota) |> pull(diff)
  tibble(x = x, y = 2 * pnorm( (180/pi) * atan(R.diff/x)/sigma) - 1)
}

tibble(sigma = 2**seq(0,5)) |> 
  mutate(curva = map(sigma, curva_angulo), 
         Sigma = as.factor(sigma)) |> 
  unnest(curva) |> 
  ggplot(aes(x = x, y = y)) + 
    geom_line(aes(group = sigma, color = Sigma)) + 
    scale_color_viridis_d() + ylim(c(0,1)) + xlab("Distancia (cm)") + ylab("Probabilidad de éxito") + 
  ggtitle(expression(paste("Probabilidad de éxito para diferentes valores de ",
                           sigma," (en grados ", ~degree, ").")), )+ sin_lineas +
  theme(plot.title = element_text(size = 15))

simula_tiros <- function(sigma){
  distancia  <- 1
  n_muestras <- 250
  angulos_tiro <- (pi/180) * rnorm(n_muestras, 0, sigma)
  tibble(x = distancia * cos(angulos_tiro), 
         y = distancia * sin(angulos_tiro))
}

tibble(sigma_grados = c(1, 8, 32, 64)) |> 
  mutate(tiros = map(sigma_grados, simula_tiros)) |> 
  unnest(tiros) |> 
  ggplot(aes(x = x, y = y)) + 
    geom_point() +
    geom_segment(aes(x = 0, y = 0, xend = x, yend = y), alpha = .1) + 
    geom_point(aes(x = 0, y = 0), color = 'red') + 
    facet_wrap(~sigma_grados, ncol = 4) + 
    ylab("") + xlab("") + ggtitle("Posiciones finales de tiro")+ sin_lineas +
  coord_equal()

data_list$r = radios$pelota
data_list$R = radios$hoyo

ruta <- file.path("modelos/golf/modelo-angulo.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

ajuste <- ajustar_modelo(modelo, data_list)
ajuste$summary() |> print(width = 75)

muestras <- tibble(posterior::as_draws_df(ajuste$draws(c("sigma", "sigma_degrees"))))

muestras |> 
  select(-sigma_degrees) |> 
  pivot_longer(cols = c(sigma), names_to = 'parameter') |> 
  mutate(Chain = as.factor(.chain)) |> 
  ggplot(aes(x = .iteration, y = value)) + 
    geom_line(aes(group = .chain, color = Chain)) + 
    facet_wrap(~parameter, ncol = 1, scales = 'free', strip.position="right") + 
  scale_color_viridis_d(option = 'plasma')+ sin_lineas

modelo_angulo <- function(sigma_radianes){
  x <- seq(0, 1.1 * max(datos$x), length.out = 50)
  R.diff <- radios |> summarise(diff = hoyo - pelota) |> pull(diff)
  tibble(x = x, y = 2 * pnorm( atan(R.diff/x)/sigma_radianes) - 1)
}

curvas_regresion <- muestras |> 
  mutate(curva = map(sigma, modelo_angulo)) |> 
  select(-sigma_degrees, -sigma) |> 
  unnest(curva) |> 
  group_by(x) |> 
  summarise(mediana = median(y), 
            q_low = quantile(y, .005), 
            q_hi = quantile(y, .995), 
            .groups = 'drop')

g_angulo <- datos |> 
  ggplot(aes(x = x, y = y/n)) + 
    geom_linerange(aes(ymin = y/n - 2 * se, ymax = y/n + 2*se)) + 
    geom_point(colour = "steelblue", alpha = 1.) + 
    geom_line(data = curvas_regresion, aes(x = x, y = mediana)) +
    geom_ribbon(data = curvas_regresion, aes(x = x, ymin = q_low, ymax = q_hi), 
                alpha = .2, inherit.aes = FALSE) +
    ylim(c(0,1)) + xlab("Distancia (cm)") + ylab("Tasa de éxito") + 
    ggtitle("Modelo con ángulo de tiro")+ sin_lineas

g_logistico + g_angulo

datos_grande <- read_delim("datos/golf_grande.csv", delim = "\t")
datos_grande <- datos_grande |> 
  mutate(x = dis * 30.48, n = count, y = exitos, se = sqrt((y/n)*(1-y/n)/n), fuente = "Nuevos") |> 
  select(x, n, y, se, fuente)

datos <- rbind(datos |> mutate(fuente = "Original"), datos_grande)
datos <- datos |> mutate(fuente = as.factor(fuente))

curvas_regresion <- muestras |> 
  mutate(curva = map(sigma, modelo_angulo)) |> 
  select(-sigma_degrees, -sigma) |> 
  unnest(curva) |> 
  group_by(x) |> 
  summarise(mediana = median(y), 
            q_low = quantile(y, .005), 
            q_hi = quantile(y, .995), 
            .groups = 'drop')

datos |> 
  ggplot(aes(x = x, y = y/n)) + 
    geom_linerange(aes(ymin = y/n - 2 * se, ymax = y/n + 2 * se)) + 
    geom_point(aes(colour = fuente), alpha = 1.) +
    geom_line(data = curvas_regresion, aes(x = x, y = mediana)) +
    geom_ribbon(data = curvas_regresion, aes(x = x, ymin = q_low, ymax = q_hi),
                alpha = .2, inherit.aes = FALSE) +
    ylim(c(0,1)) + xlab("Distancia (cm)") + ylab("Tasa de éxito") +
    ggtitle("Modelo con ángulo de tiro")+ sin_lineas

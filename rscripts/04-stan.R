## Setup ---------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 4)
## Problemas con mi consola en Emacs
options(pillar.subtle = FALSE, pillar.width = 75)
options(rlang_backtrace_on_error = "none")
options(crayon.enabled = FALSE)

## Para el tema de ggplot
sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Setup ---------------------------------------------------------------------
library(cmdstanr)
library(posterior)
library(bayesplot)

## Funciones auxiliares ------------------------------------------------------
print_file <- function(file) {
  cat(paste(readLines(file), "\n", sep=""), sep="")
}

## Modelo Beta-Binomial --------------------------------------------------------
modelos_files <- "modelos/compilados/tutorial"
ruta <- file.path("modelos/tutorial/beta-binomial.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

class(modelo)

data.list <- list(Y = 7)

muestras <- modelo$sample(data = data.list, 
                          chains = 1, 
                          iter=1500, 
                          iter_warmup=500, 
                          seed=483892929, 
                          refresh=500)

class(muestras)

mcmc_trace(muestras$draws(), pars = "theta") +
  sin_lineas +
geom_hline(yintercept = 9/14, lty = 2, color = 'black')

# Histogram of the Markov chain values
g1 <- mcmc_hist(muestras$draws(), pars = "theta") + 
  yaxis_text(TRUE) + 
  ylab("count") + sin_lineas

# Density plot of the Markov chain values
g2 <- mcmc_dens(muestras$draws(), pars = "theta") + 
  yaxis_text(TRUE) + 
  ylab("density") + sin_lineas

g1 + g2

## Modelo BetaBinomial Conjugado -----------------------------------------------
modelos_files <- "modelos/compilados/tutorial"
ruta <- file.path("modelos/tutorial/beta-binomial-conjugado.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

muestras <- modelo$sample(data   = data.list, 
                          chains = 1, 
                          iter   = 1500, 
                          iter_warmup = 500, 
                          seed   = 10101, 
                          refresh= 500,
                          fixed_param = TRUE)

# Histogram of the Markov chain values
g1 <- mcmc_hist(muestras$draws(), pars = "theta") + 
  yaxis_text(TRUE) + 
  ylab("count") + sin_lineas

# Density plot of the Markov chain values
g2 <- mcmc_dens(muestras$draws(), pars = "theta") + 
  yaxis_text(TRUE) + 
  ylab("density") + sin_lineas

g1 + g2

## Caso: escuelas --------------------------------------------------------------
data <- tibble( id = factor(seq(1, 8)), 
                y = c(28, 8, -3, 7, -1, 1, 18, 12), 
                sigma = c(15, 10, 16, 11, 9, 11, 10, 18))

modelos_files <- "modelos/compilados/caso-escuelas"
ruta <- file.path("modelos/caso-escuelas/modelo-escuelas.stan")
modelo <- cmdstan_model(ruta, dir = modelos_files)

data_list <- c(data, J = 8)

muestras <- modelo$sample(data = data_list, 
                          chains = 1, 
                          iter=700, 
                          iter_warmup=500, 
                          seed=483892929, 
                          refresh=1200)

muestras$cmdstan_diagnose()

muestras

muestras$cmdstan_summary()

## Ejemplo de código utilizando Rstan
stanfit <- rstan::read_stan_csv(muestras$output_files())
stanfit

muestras_dt <- tibble(posterior::as_draws_df(muestras$draws(c("tau", "theta"))))

g_tau <- muestras_dt |> 
   ggplot(aes(x = .iteration, y = log(tau))) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(-4, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)

g_theta <- muestras_dt |> 
   ggplot(aes(x = .iteration, y =`theta[1]`)) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    geom_hline(yintercept = 0.7657852, lty = 2)
g_tau /g_theta

muestras_dt |> 
   mutate(media = cummean(log(tau))) |> 
   ggplot(aes(x = .iteration, y = media)) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(-4, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)

g1_dispersion <- muestras_dt |> 
  mutate(log_tau = log(tau)) |> 
  mcmc_scatter(
  pars = c("theta[1]", "log_tau"),
  np = nuts_params(muestras),
  np_style = scatter_style_np(div_color = "salmon", div_alpha = 0.8)
) + sin_lineas+ ylim(-4, 3)
g1_dispersion

mcmc_parcoord(muestras_dt |> select(-.chain, -.iteration, -.draw), 
              transform = list(tau = "log"),
              np = nuts_params(muestras), 
              np_style = scatter_style_np(div_color = "salmon", 
                                          div_alpha = 0.5, 
                                          div_size = .5)) + 
  sin_lineas

acf_theta <- mcmc_acf(muestras_dt, pars = "theta[1]", lags = 10) + sin_lineas
acf_tau   <- mcmc_acf(muestras_dt, pars = "tau", lags = 10) + sin_lineas

acf_tau / acf_theta

muestras <- modelo$sample(data        = data_list, 
                          chains      = 1, 
                          iter        = 5000, 
                          iter_warmup = 5000, 
                          seed        = 483892929, 
                          refresh     = 10000)

muestras_dt <- tibble(posterior::as_draws_df(muestras$draws(c("tau", "theta[1]"))))
muestras_dt |> 
   ggplot(aes(x = .iteration, y = log(tau))) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(-4, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)

g2_dispersion <- muestras_dt |> 
  mutate(log_tau = log(tau)) |> 
  mcmc_scatter(
  pars = c("theta[1]", "log_tau"),
  np = nuts_params(muestras),
  np_style = scatter_style_np(div_color = "salmon", div_alpha = 0.8)) + 
  sin_lineas+ ylim(-4, 3) +
  ggtitle("Original")

g2_dispersion

muestras_dt |> 
   mutate(media = cummean(log(tau))) |> 
   ggplot(aes(x = .iteration, y = media)) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(0, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)

muestras <- modelo$sample(data        = data_list, 
                          chains      = 1, 
                          iter        = 5000, 
                          iter_warmup = 5000, 
                          seed        = 483892929, 
                          refresh     = 10000, 
                          adapt_delta = .90)

muestras_dt <- tibble(posterior::as_draws_df(muestras$draws(c("tau", "theta[1]"))))

g1 <- muestras_dt |> 
   ggplot(aes(x = .iteration, y = log(tau))) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(-4, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)


g2_dispersion_90 <- muestras_dt |> 
  mutate(log_tau = log(tau)) |> 
  mcmc_scatter(
  pars = c("theta[1]", "log_tau"),
  np = nuts_params(muestras),
  np_style = scatter_style_np(div_color = "salmon", div_alpha = 0.8)) + 
  sin_lineas + ylim(-4, 3) +
  ggtitle("Configuración hmc")

g1 / (g2_dispersion + g2_dispersion_90)

## Cambio de parametrización ---------------------------------------------------
ruta_ncp <- file.path("modelos/caso-escuelas/modelo-escuelas-ncp.stan")
modelo_ncp <- cmdstan_model(ruta_ncp, dir = modelos_files)

muestras_ncp <- modelo_ncp$sample(data = data_list, 
                          chains = 1, 
                          iter=5000, 
                          iter_warmup=5000, 
                          seed=483892929, 
                          refresh=10000)

print(muestras_ncp, max_rows = 19)

muestras_dt <- tibble(posterior::as_draws_df(muestras_ncp$draws(c("tau", "theta[1]", "theta_tilde[1]"))))

muestras_dt |> 
   ggplot(aes(x = .iteration, y = log(tau))) + 
    geom_point() + sin_lineas + 
    xlab("Iteraciones") + 
    ylim(-4, 4) + 
    geom_hline(yintercept = 0.7657852, lty = 2)

g3 <- muestras_dt |> 
  mutate(log_tau = log(tau)) |> 
  mcmc_scatter(
  pars = c("theta_tilde[1]", "log_tau"),
  np = nuts_params(muestras_ncp),
  np_style = scatter_style_np(div_color = "salmon", div_alpha = 0.8)) + 
  sin_lineas + ylim(-4, 3) +
  ggtitle("Variable auxiliar")

g3_dispersion <- muestras_dt |> 
  mutate(log_tau = log(tau)) |> 
  mcmc_scatter(
  pars = c("theta[1]", "log_tau"),
  np = nuts_params(muestras_ncp),
  np_style = scatter_style_np(div_color = "salmon", div_alpha = 0.8)) + 
  sin_lineas + ylim(-4, 3) +
  ggtitle("Re-parametrización")

g3 + g3_dispersion

g2_dispersion + g2_dispersion_90 + g3_dispersion

## Modelos de regularizacion ---------------------------------------------------
modelos_files <- "modelos/compilados/regularizacion"
ruta <- file.path("modelos/regularizacion/modelo-")

compila_modelo <- function(modelo){
  modelo_name <- paste(ruta, modelo, ".stan",sep = "")
  cmdstan_model(modelo_name, dir = modelos_files)
}

genera_muestras <- function(modelo){
  modelo$sample(data = data.list, 
                chains = 1, 
                iter=5000, 
                iter_warmup=500, 
                seed=483892929, 
                refresh=700)
}

data.list <- list(p = 2, sigma = 1)

tibble(nombre = fct_inorder(c("normal", "laplace", "horseshoe"))) |>
  mutate(modelo = map(nombre, compila_modelo),
         ajuste = map(modelo, genera_muestras),
         muestras = map(ajuste, function(x){ as_draws_df(x$draws()) })) |>
  unnest(muestras) |>
  ggplot(aes(`theta[1]`, `theta[2]`)) +
  geom_point(size = 1, alpha = .2) +
  facet_wrap(~nombre) + sin_lineas + coord_equal() +
  xlim(-10, 10) + ylim(-10, 10) +
  ylab(expression(theta[2])) + xlab(expression(theta[1]))

## Ejemplo regresion regularizada ----------------------------------------------
library(rstanarm)
library(bayesplot)
data <- read_csv("datos/diabetes.csv")
# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
      data <- data[-which(data[, i] == 0), ]
}
# scale the covariates for easier comparison of coefficient posteriors
for (i in 1:8) {
      data[i] <- scale(data[i])
}
# modify the data column names slightly for easier typing
names(data)[7] <- "dpf"
names(data) <- tolower(names(data))
data$outcome <- factor(data$outcome)

n=dim(data)[1]
p=dim(data)[2]

(reg_formula <- formula(paste("outcome ~", paste(names(data)[1:(p-1)], collapse = " + "))))

model.normal <- stan_glm(reg_formula, data, family = binomial(link = "logit"))

g1 <- plot(model.normal, "areas", prob = 0.95, prob_outer = 1) +
  geom_vline(xintercept = 0, lty = 2) + ggtitle("Normal") + sin_lineas

model.laplace <- stan_glm(reg_formula, data, family = binomial(link = "logit"),
                            prior = laplace())
model.horseshoe <- stan_glm(reg_formula, data, family = binomial(link = "logit"),
                            prior = hs())

g2 <- plot(model.laplace, "areas", prob = 0.95, prob_outer = 1) +
  geom_vline(xintercept = 0, lty = 2) + ggtitle("Laplace") + sin_lineas
g3 <- plot(model.horseshoe, "areas", prob = 0.95, prob_outer = 1) +
  geom_vline(xintercept = 0, lty = 2) + ggtitle("Horseshoe") + sin_lineas

g1 + g2 + g3

mcmc_scatter(model.horseshoe,
           pars = c("pregnancies", "skinthickness"),
           np   = nuts_params(model.horseshoe),
           alpha = 0.2) + sin_lineas

## Ejemplo mtcars ------------------------------------
fit <- stan_glm(
  mpg ~ ., data = mtcars,
  iter = 1000, refresh = 0,
  # this combo of prior and adapt_delta should lead to some divergences
  prior = hs(),
  adapt_delta = 0.9
)

posterior <- as.array(fit)
np <- nuts_params(fit)

# mcmc_scatter with divergences highlighted
mcmc_scatter(posterior, pars = c("wt", "sigma"), np = np, alpha = .3) + sin_lineas

library("rstanarm")
x <- seq(-2,2,1)
y <- c(50, 44, 50, 47, 56)
sexratio <- tibble(x, y)

fit <- lm(y ~ x, data = sexratio)
fit |> broom::tidy()
fit |> broom::glance() |> select(1:5)

g1 <- sexratio |>
    ggplot(aes(x, y)) +
    geom_point() +
    xlab("Medida de atracción") + ylab("Porcentaje de niñas") + sin_lineas

  g2 <- sexratio |>
    ggplot(aes(x, y)) +
    geom_point() +
    geom_smooth(formula = y ~ x, sd = TRUE, method = "lm") + 
    xlab("Medida de atracción") + ylab("Porcentaje de niñas") + sin_lineas

g1 + g2

fit_post <- stan_glm(y ~ x, data = sexratio,
                     prior = normal(0, 0.2),
                     prior_intercept = normal(48.8, 0.5),
                     refresh = 0)
prior_summary(fit_post)

print(fit_post)

fit_default <- stan_glm(y ~ x, data = sexratio, refresh = 0)
prior_summary(fit_default)

gweak <- g1 +
  geom_abline(data = as_tibble(fit_default) |> mutate(id = 1:n()) |>
                sample_n(100),
              aes(slope = x, intercept = `(Intercept)`, group = id),
              alpha = .2) +
  geom_abline(data = as_tibble(fit_default) |> pivot_longer(1:3) |>
                group_by(name) |> summarise(.estimate = mean(value)) |>
                pivot_wider(values_from = .estimate),
              aes(slope = x, intercept = `(Intercept)`),
              linewidth = 2)

ginformative <- g1 +
  geom_abline(data = as_tibble(fit_post) |> mutate(id = 1:n()) |>
                sample_n(100),
              aes(slope = x, intercept = `(Intercept)`, group = id),
              alpha = .2) +
  geom_abline(data = as_tibble(fit_post) |> pivot_longer(1:3) |>
                group_by(name) |> summarise(.estimate = mean(value)) |>
                pivot_wider(values_from = .estimate),
              aes(slope = x, intercept = `(Intercept)`),
              linewidth = 2)

gweak + ginformative

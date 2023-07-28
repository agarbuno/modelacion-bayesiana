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
log_lik <- posterior.indep$draws("log_lik")
r_eff <- relative_eff(posterior.indep$draws("log_lik") |> exp(), cores = 2)

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

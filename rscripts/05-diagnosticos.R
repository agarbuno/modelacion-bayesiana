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

library(mvtnorm)
library(R6)
ModeloNormalMultivariado <-
  R6Class("ProbabilityModel",
          list(
            mean = NA,
            cov  = NA, 
            initialize = function(mu = 0, sigma = 1){
              self$mean = mu
              self$cov  = sigma |> as.matrix()
            }, 
            sample = function(n = 1){
              rmvnorm(n, mean = self$mean, sigma = self$cov)              
            },
            density = function(x, log = TRUE){
              dmvnorm(x, self$mean, self$cov, log = log)              
            }           
          ))

### Muestreador Metropolis-Hastings --------------------------------------------
crea_metropolis_hastings <- function(objetivo, muestreo){
  ## Este muestreador aprovecha la simetría de la propuesta 
  function(niter, x_start){
    ## Empezamos en algun lugar
    estado <- x_start
    ndim <- length(estado) 
    muestras <- matrix(nrow = niter, ncol = ndim + 1)      
    muestras[1,2:(ndim+1)] <- estado
    muestras[1,1] <- 1
    for (ii in 2:niter){
      propuesta   <- estado + muestreo$sample()
      log_pi_propuesta <- objetivo$density(propuesta)
      log_pi_estado    <- objetivo$density(estado)
      log_alpha <- log_pi_propuesta - log_pi_estado

      if (log(runif(1)) < log_alpha) {
        muestras[ii, 1] <- 1 ## Aceptamos
        muestras[ii, 2:(ndim+1)] <- propuesta
      } else {
        muestras[ii, 1] <- 0 ## Rechazamos
        muestras[ii, 2:(ndim+1)] <- estado
      }
      estado <- muestras[ii, 2:(ndim+1)]
    }
    if (ndim == 1) {colnames(muestras) <- c("accept", "value")}
    muestras
  }
}

set.seed(108727)
mu <- c(0, 0)
Sigma <- matrix(c(1, .75, .75, 1), nrow = 2)
objetivo <- ModeloNormalMultivariado$new(mu, Sigma)
muestreo <- ModeloNormalMultivariado$new(c(0,0),  .05 * diag(2))

muestras <- tibble(id = factor(1:5), x1 = c(-2, 2, 2, -2, 0), x2 = c(2, -2, 2, -2, 0)) |>
  nest(x_start   = c(x1,x2)) |>
  mutate(cadenas = map(x_start, function(x0){
    mcmc <- crea_metropolis_hastings(objetivo, muestreo)
    mcmc(1000, c(x0$x1, x0$x2)) |>
      as_tibble() |>
      mutate(iter = 1:1000)
  }))

g.corta <- muestras |>
    unnest(cadenas) |>
    filter(iter <= 50) |>
    ggplot(aes(V2, V3, color = id)) +
    geom_path() + geom_point(size = .3) +
    geom_point(data = muestras |> unnest(x_start), aes(x1, x2), color = 'red') + 
    xlab(expression(x[1])) + ylab(expression(x[2])) + 
    sin_lineas + sin_leyenda + ylim(-3,3) + xlim(-3,3)


  g.completa <- muestras |>
    unnest(cadenas) |>
    ggplot(aes(V2, V3, color = id)) +
    geom_path() + geom_point(size = .3) +
    geom_point(data = muestras |> unnest(x_start), aes(x1, x2), color = 'red') + 
    xlab(expression(x[1])) + ylab(expression(x[2])) + 
    sin_lineas + sin_leyenda + ylim(-3,3) + xlim(-3,3)

  g.conjunta <- muestras |>
    unnest(cadenas) |>
    ggplot(aes(V2, V3)) +
    geom_point(size = .3) +
    geom_point(data = muestras |> unnest(x_start), aes(x1, x2), color = 'red') + 
    xlab(expression(x[1])) + ylab(expression(x[2])) + 
    sin_lineas + sin_leyenda + ylim(-3,3) + xlim(-3,3)

g.objetivo <- objetivo$sample(4000) |>
  as_tibble() |>
  ggplot(aes(V1, V2)) +
    geom_point(size = .3) +
    xlab(expression(x[1])) + ylab(expression(x[2])) + 
    sin_lineas + sin_leyenda + ylim(-3,3) + xlim(-3,3)

  (g.corta + g.completa) / (g.conjunta + g.objetivo)

muestreo <- ModeloNormalMultivariado$new(c(0,0),  10 * diag(2))

muestras.mal <- tibble(id = factor(1:5), x1 = c(-2, 2, 2, -2, 0), x2 = c(2, -2, 2, -2, 0)) |>
  nest(x_start   = c(x1,x2)) |>
  mutate(cadenas = map(x_start, function(x0){
    mcmc <- crea_metropolis_hastings(objetivo, muestreo)
    mcmc(1000, c(x0$x1, x0$x2)) |>
      as_tibble() |>
      mutate(iter = 1:1000)
  }))

g1 <- muestras |>
  unnest(cadenas) |>
  ggplot(aes(iter, V2, color = id)) +
  geom_line() + sin_lineas + sin_leyenda +
  ylab(expression(x[1]))


g2 <- muestras.mal |>
  unnest(cadenas) |>
  ggplot(aes(iter, V2, color = id)) +
  geom_line() + sin_lineas + sin_leyenda +
  ylab(expression(x[1]))

g1/g2

## Datos: cantantes de opera -----------------------
set.seed(3413)
cantantes <- lattice::singer |>
  mutate(estatura_cm = round(2.54 * height)) |>
  filter(str_detect(voice.part, "Tenor")) |>
  select(voice.part, estatura_cm) |>
  sample_n(20) |>
  as_tibble()

cantantes |> print(n = 3)

ModeloNormal <-
  R6Class("PosteriorProbabilityModel",
          list(
            observaciones = NA,
            mu_0 = NA, n_0 = NA, a = NA, b = NA,
            initialize = function(x = 0){
              ## Observaciones
              self$observaciones <- x
              ## Previa
              self$mu_0 <- 175
              self$n_0  <- 5
              self$a    <- 3
              self$b    <- 140
            },
            density = function(theta, log = TRUE){
              theta <- matrix(theta, nrow = 1)
              verosimilitud <- sum(dnorm(self$observaciones, theta[1], sd = theta[2], log = log))
              previa <- dnorm(theta[1], self$mu_0, sd = theta[2]/sqrt(self$n_0), log = log) +
                dgamma(1/(theta[2]**2), self$a, self$b, log = log)
              verosimilitud + previa 
            }           
          ))

objetivo <- ModeloNormal$new(cantantes$estatura_cm)
muestreo <- ModeloNormalMultivariado$new(c(0,0),  0.50 * diag(2))

set.seed(108727)
mcmc <- crea_metropolis_hastings(objetivo, muestreo)

muestras.cantantes <-  mcmc(5000, c(162, 3)) |>
  as_tibble() |>
  mutate(mu = V2, sigma = V3, iter = 1:n())

muestras.cantantes |>
  ggplot(aes(mu, sigma, color = iter)) +
  geom_line(alpha = .2) +geom_point(size = 4, alpha = .4) + 
  sin_lineas

cadenas.cantantes <- tibble(cadena  = factor(1:4),
        mu_start    = rnorm(4, 160, 20),
        sigma_start = runif(4, 0, 20)) |>
   nest(inicial = c(mu_start, sigma_start)) |>
   mutate(cadenas = map(inicial, function(x0){
     mcmc(2500, c(x0$mu_start, x0$sigma_start)) |>
       as_tibble() |>
       mutate(mu = V2, sigma = V3, iter = 1:n())
   }))

cadenas.cantantes |>
   unnest(cadenas) |>
   pivot_longer(cols = mu:sigma) |>
   ggplot(aes(iter, value, color = cadena)) +
   geom_line() +
   facet_wrap(~name, ncol = 1, scales = "free_y") +
   sin_lineas

cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter >= 1000) |> 
  pivot_longer(cols = mu:sigma) |>
  ggplot(aes(iter, value, color = cadena)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  sin_lineas

cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter > 100) |> 
  summarise(.estimate = mean(sigma), .variance = var(sigma), .error_mc = sqrt(.variance/n()))

cadenas.cantantes |>
 unnest(cadenas) |>
 filter(iter > 100) |> 
 group_by(cadena) |> 
 summarise(media = mean(sigma), varianza = var(sigma)) |>
 summarise(.estimate = mean(media), .error_mc = sd(media))

cadenas.cantantes |>
 unnest(cadenas) |>
 filter(iter > 100) |> 
 group_by(cadena) |> 
 summarise(media = mean(sigma), varianza = var(sigma))

cadenas.cantantes |>
    unnest(cadenas) |>
    filter(iter < 300) |>
    ggplot(aes(x = iter, y = mu, color = cadena)) + 
    geom_path() +  sin_lineas + 
    annotate("rect", xmin = 0, xmax = 225, ymin = -Inf, ymax = Inf, alpha = .2) + 
    annotate("rect", xmin = 0, xmax = 150, ymin = -Inf, ymax = Inf, alpha = .2) + 
    annotate("text", x = c(75, 187.5,262.5),
             y = rep(145, 3), 
             label = c("burn-in", "sub 1", "sub 2"))

diagnosticos.rhat.short <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 200) |>
  filter(iter > max(iter)/2) |>
  mutate(cadena = paste(cadena, ifelse(iter <= (max(iter) + min(iter))/2, 
                                       'a', 'b'), sep = "")) |>
  pivot_longer(mu:sigma, names_to = "parametro", values_to = "valor") |>
  group_by(parametro, cadena) |>
  summarise(media = mean(valor), num = n(), sigma2 = var(valor)) |>
  summarise(N = first(num), 
            M = n_distinct(cadena), 
            B = N * var(media), 
            W = mean(sigma2), 
            V_hat = ((N-1)/N) * W + B/N,
            R_hat = sqrt(V_hat/W)) 

g.mu <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 200) |>
  ggplot(aes(x = iter, y = mu, color = cadena)) + 
  geom_path() + sin_leyenda + sin_lineas + 
  ggtitle(paste("Rhat: ", round((diagnosticos.rhat.short |> pull(R_hat))[1], 3), sep = "")) + 
  annotate("rect", xmin = 0, xmax = 150, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("rect", xmin = 0, xmax = 100, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("text", x = c(50, 125, 175),
           y = rep(145, 3), 
           label = c("burn-in", "sub 1", "sub 2"))

g.sigma <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 200) |>
  ggplot(aes(x = iter, y = sigma, color = cadena)) + 
  geom_path() + sin_leyenda + sin_lineas + 
  ggtitle(paste("Rhat: ", round((diagnosticos.rhat.short |> pull(R_hat))[2], 3), sep = "")) + 
  annotate("rect", xmin = 0, xmax = 150, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("rect", xmin = 0, xmax = 100, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("text", x = c(50, 125, 175),
           y = rep(5, 3), 
           label = c("burn-in", "sub 1", "sub 2"))

g.mu / g.sigma

diagnosticos.rhat.short <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 600) |>
  filter(iter > max(iter)/2) |>
  mutate(cadena = paste(cadena, ifelse(iter <= (max(iter) + min(iter))/2, 
                                       'a', 'b'), sep = "")) |>
  pivot_longer(mu:sigma, names_to = "parametro", values_to = "valor") |>
  group_by(parametro, cadena) |>
  summarise(media = mean(valor), num = n(), sigma2 = var(valor)) |>
  summarise(N = first(num), 
            M = n_distinct(cadena), 
            B = N * var(media), 
            W = mean(sigma2), 
            V_hat = ((N-1)/N) * W + B/N,
            R_hat = sqrt(V_hat/W)) 

g.mu <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 600) |>
  ggplot(aes(x = iter, y = mu, color = cadena)) + 
  geom_path() + sin_leyenda + sin_lineas + 
  ggtitle(paste("Rhat: ", round((diagnosticos.rhat.short |> pull(R_hat))[1], 3), sep = "")) + 
  annotate("rect", xmin = 0, xmax = 450, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("rect", xmin = 0, xmax = 300, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("text", x = c(150, 375, 525),
           y = rep(145, 3), 
           label = c("burn-in", "sub 1", "sub 2"))

g.sigma <- cadenas.cantantes |>
  unnest(cadenas) |>
  filter(iter < 600) |>
  ggplot(aes(x = iter, y = sigma, color = cadena)) + 
  geom_path() + sin_leyenda + sin_lineas + 
  ggtitle(paste("Rhat: ", round((diagnosticos.rhat.short |> pull(R_hat))[2], 3), sep = "")) + 
  annotate("rect", xmin = 0, xmax = 450, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("rect", xmin = 0, xmax = 300, ymin = -Inf, ymax = Inf, alpha = .2) + 
  annotate("text", x = c(150, 375, 525),
           y = rep(5, 3), 
           label = c("burn-in", "sub 1", "sub 2"))

g.mu / g.sigma

library(forecast)
ggAcf(rgamma(1000,1,1)) + sin_lineas

ggAcf(muestras.cantantes$mu) + sin_lineas +
ggtitle("Series: mu (modelo cantantes)")

library(posterior)
c(mu    = ess_basic(muestras.cantantes$mu)/nrow(muestras.cantantes),
  sigma = ess_basic(muestras.cantantes$sigma)/nrow(muestras.cantantes),
  accept = mean(muestras.cantantes$V1))

### Actualización del muestreador  -------------------------------------------
set.seed(108727)
objetivo <- ModeloNormal$new(cantantes$estatura_cm)
muestreo <- ModeloNormalMultivariado$new(c(0,0), 3 * diag(2))
mcmc <- crea_metropolis_hastings(objetivo, muestreo)

muestras.cantantes <-  mcmc(5000, c(175, 6.5)) |>
  as_tibble() |>
  mutate(mu = V2, sigma = V3, iter = 1:n())

c(mu    = ess_basic(muestras.cantantes$mu)/nrow(muestras.cantantes),
  sigma = ess_basic(muestras.cantantes$sigma)/nrow(muestras.cantantes),
  accept = mean(muestras.cantantes$V1))

ggAcf(muestras.cantantes$mu) + sin_lineas +
  ggtitle("Series: mu (modelo cantantes)")

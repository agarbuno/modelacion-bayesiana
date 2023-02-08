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

## Muestreo por aceptacion rechazo ---------------

crea_mezcla <- function(weights){
  function(x){
    weights$w1 * dnorm(x, mean = -1.5, sd = .5) +
      weights$w2 * dnorm(x, mean = 1.5, sd = .7)
  }
}

objetivo <- crea_mezcla(list(w1 = .6, w2 = .4))

tibble(x = seq(-5, 5, length.out = 100)) |>
  mutate(y = objetivo(x),
         aprox = 3.3 * dnorm(x, 0, sd = 2)) |>
  ggplot(aes(x,y)) +
  geom_area(fill = "lightblue") +
  geom_line(aes(x, aprox), lty = 2) +
  geom_ribbon(aes(ymin = y, ymax = aprox), fill = "salmon") + sin_lineas +
  sin_ejes

crea_mezcla <- function(weights){
  function(x){
    weights$w1 * dnorm(x, mean = -1.5, sd = .5) +
      weights$w2 * dnorm(x, mean = 1.5, sd = .7)
  }
}
objetivo <- crea_mezcla(list(w1 = .6, w2 = .4))
M        <- 3.3

library(R6)
ModeloNormal <-
  R6Class("ProbabilityModel",
          list(
            mean = NA,
            sd = NA,
            ## Inicializador
            initialize = function(mean = 0, sd = 1){
              self$mean = mean
              self$sd   = sd
            },
            ## Muestreador
            sample = function(n = 1){
              rnorm(n, self$mean, sd = self$sd)              
            },
            ## Evaluacion de densidad
            density = function(x, log = TRUE){
              dnorm(x, self$mean, sd = self$sd, log = log)
            }           
          ))

crea_rejection_sampling <- function(objetivo, aprox, M){
  function(niter){
    muestras <- matrix(nrow = niter, ncol = 3)
    for (ii in seq(1, niter)){
      propuesta <- aprox$sample()
      p <- objetivo(propuesta)
      g <- aprox$density(propuesta, log = FALSE)
      u <- runif(1)
      if (u < p/(M * g)) {  ## Aceptamos 
        muestras[ii, 1] <- 1
      } else {              ## Rechazamos 
        muestras[ii, 1] <- 0
      }
      muestras[ii, 2] <- propuesta
      muestras[ii, 3] <- u 
    }
    colnames(muestras) <- c("accept", "value", "auxiliar")
    muestras
  }
}

modelo.muestreo  <- ModeloNormal$new(mean = 0, sd = 2)
muestreo_rechazo <- crea_rejection_sampling(objetivo, modelo.muestreo, M)

muestras <- muestreo_rechazo(5000) |>
  as.tibble() |>
  mutate(density = modelo.muestreo$density(value, log = FALSE))

g1 <- muestras |>
  ggplot(aes(value, auxiliar * modelo.muestreo$density(value, log = FALSE))) +
  geom_point(aes(color = factor(accept))) + sin_lineas + sin_ejes + sin_leyenda +
  xlab("") + ylab("") +
  ggtitle(paste("Muestras en el espacio (x,u), aceptación: ", mean(muestras$accept)))

g2 <- muestras |>
  filter(accept == 1) |>
  ggplot(aes(value)) +
  geom_histogram() + 
  sin_lineas + sin_ejes + sin_leyenda +
  xlab("") + ylab("") +
  ggtitle("Histograma de las muestras generadas")

g1 + g2

## Caminata entre islas --------------------------
set.seed(1087)

islas <- tibble(islas = 1:7, pob = c(1,2,3,4,5,4,3))
camina_isla <- function(i){ # i: isla actual
  u_izq <- runif(1) # Lanzamos volado para ver si nos vamos izq o der. 
  v <- ifelse(u_izq < 0.5, i - 1, i + 1)  # Pedimos índice isla vecina. 
  if (v < 1 | v > 7) { # si estas en los extremos y el volado indica salir
    return(i)
  }
  u_cambio <- runif(1) # Moneda de aceptacion de cambio
  p_cambio = min(islas$pob[v]/islas$pob[i], 1)
  if (u_cambio < p_cambio) {
    return(v) # isla destino
  }
  else {
    return(i) # me quedo en la misma isla
  }
}

pasos <- 100000; iteraciones <- numeric(pasos)
iteraciones[1] <- sample(1:7, 1) # isla inicial
for (j in 2:pasos) {
    iteraciones[j] <- camina_isla(iteraciones[j - 1])
}
caminata <- tibble(paso = 1:pasos, isla = iteraciones)

plot_caminata <- ggplot(caminata[1:500, ], aes(x = paso, y = isla)) +
  geom_point(size = 0.8) +
  geom_path(alpha = 0.5) +
  labs(title = "Caminata aleatoria") +
  scale_x_continuous(trans = "log10", "Tiempo", breaks = c(1, 2, 5, 20, 100, 500)) +
  scale_y_continuous( expression(theta)) + sin_lineas
plot_dist <- ggplot(caminata, aes(x = isla)) +
  geom_bar(fill = "darkgray", aes(y = (..count..)/sum(..count..))) +
  geom_bar(data = islas |>  mutate(prop = pob/sum(pob)),
           aes(x = islas, y = prop), fill = "steelblue", alpha = .3, stat = "identity") + 
  scale_x_continuous(expression(theta), breaks = 1:10) +
  ylim(0,.5) + 
  labs(title = "Distribución objetivo (Histograma)", 
       y = expression(hat(pi)(theta))) + sin_lineas + coord_flip()
plot_caminata + plot_dist

## Caminata en espacio continuo ------------------------

ModeloUniforme <-
  R6Class("ProbabilityModel",
          list(
            a = NA,
            b = NA, 
            initialize = function(a = 0, b = 1){
              self$a = a
              self$b = b
            }, 
            sample = function(n = 1){
              runif(n, self$a, self$b)              
            },
            density = function(x, log = TRUE){
              dunif(x, self$a, self$b, log = log)
            }           
          ))

crea_cadena_markov <- function(objetivo, muestreo){
  function(niter){
    muestras <- matrix(nrow = niter, ncol = 2)
    ## Empezamos en algun lugar
    estado   <- muestreo$sample()
    muestras[1,1] <- 1
    muestras[1,2] <- estado
    for (ii in 2:niter){
      ## Generamos un candidato (caminata aleatoria)
      propuesta   <- estado + muestreo$sample()
      p_propuesta <- objetivo$density(propuesta, log = FALSE)
      p_estado    <- objetivo$density(estado, log = FALSE)
      ## Evaluamos probabilidad de aceptar
      if (runif(1) < p_propuesta/p_estado) {
        muestras[ii, 1] <- 1 ## Aceptamos
        muestras[ii, 2] <- propuesta
      } else {
        muestras[ii, 1] <- 0 ## Rechazamos
        muestras[ii, 2] <- estado
      }
      estado <- muestras[ii, 2]
    }
    colnames(muestras) <- c("accept", "value")
    muestras
  }
}

objetivo <- ModeloNormal$new(mean = 4, sd = .6)
muestreo <- ModeloUniforme$new(a = -1, b = 1)

mcmc <- crea_cadena_markov(objetivo, muestreo)
muestras <- mcmc(5000)

g1 <- muestras |>
  as.tibble() |>
  mutate(iter = 1:n()) |>
  ggplot(aes(iter, value)) +
  geom_line() + sin_lineas + 
  ggtitle(paste("Trayectoria, eficiencia: ", mean(muestras[,1])))

g2 <- muestras |>
  as.tibble() |>
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = objetivo$density,
                args = list(log = FALSE),
                color = "salmon",
                size = 2) + sin_lineas + 
  ggtitle("Histograma")

g1 + g2

## Implementacion Metropolis Hastings -----------------------
ModeloGamma <-
  R6Class("ProbabilityModel",
          list(
            shape = NA,
            rate  = NA, 
            initialize = function(a = 0, b = 1){
              self$shape = a
              self$rate  = b
            }, 
            sample = function(n = 1){
              rgamma(n, shape = self$shape, rate = self$rate)              
            },
            density = function(x, log = TRUE){
              dgamma(x, shape = self$shape, rate = self$rate, log = log)
            }           
          ))

### Muestreador Metropolis-Hastings -------------------------
crea_metropolis_hastings <- function(objetivo, muestreo){
  ## Este muestreador aprovecha la simetría de la propuesta 
  function(niter){
    ## Empezamos en algun lugar
    estado <- muestreo$sample()
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
objetivo <- ModeloGamma$new(a = 20, b = 100)
muestreo <- ModeloNormal$new(sd = 0.001)
mcmc_chico <- crea_metropolis_hastings(objetivo, muestreo)

g1 <- mcmc_chico(3000) |>
  as.tibble() |>
  mutate(t = 1:n()) |>
  ggplot(aes(t, value)) +
  geom_line() + sin_lineas + ylab(expression(theta)) +
  ylim(0, 0.5)

g2 <- tibble(x = rgamma(10000, 20, 100)) |>
  ggplot(aes(y = x, x = "")) +
  geom_violin() +
  ylab("") + sin_lineas +
  ylim(0, 0.5)

g1 + g2 + plot_layout(widths = c(5, 1))

set.seed(108727)
muestreo <- ModeloNormal$new(sd = 20)
mcmc_grande <- crea_metropolis_hastings(objetivo, muestreo)

g1 <- mcmc_grande(3000) |>
  as.tibble() |>
  mutate(t = 1:n()) |>
  ggplot(aes(t, value)) +
  geom_line() + sin_lineas + ylab(expression(theta)) +
  ylim(0, 0.5)

g2 <- tibble(x = rgamma(10000, 20, 100)) |>
  ggplot(aes(y = x, x = "")) +
  geom_violin() +
  ylab("") + sin_lineas +
  ylim(0, 0.5)

g1 + g2 + plot_layout(widths = c(5, 1))

set.seed(108727)
muestreo <- ModeloNormal$new(sd = 0.1)
mcmc_justo <- crea_metropolis_hastings(objetivo, muestreo)

g1 <- mcmc_justo(3000) |>
  as.tibble() |>
  mutate(t = 1:n()) |>
  ggplot(aes(t, value)) +
  geom_line() + sin_lineas + ylab(expression(theta)) +
  ylim(0, 0.5)

g2 <- tibble(x = rgamma(10000, 20, 100)) |>
  ggplot(aes(y = x, x = "")) +
  geom_violin() +
  ylab("") + sin_lineas +
  ylim(0, 0.5)

g1 + g2 + plot_layout(widths = c(5, 1))

tibble(configuracion = c("Paso chico", "Paso grande", "Paso justo"), 
       cadena   = c(mcmc_chico, mcmc_grande, mcmc_justo)) |>
  mutate(muestras = map(cadena, function(x) {
    set.seed(108727)
    x(3000) |>
      as.tibble()
  })) |>
  unnest(muestras) |>
  group_by(configuracion) |>
  summarise(media = mean(value),
            tasa.aceptacion = mean(accept)) |>
  rbind(tibble(configuracion = "Teorica",
               media = objetivo$shape/objetivo$rate,
               tasa.aceptacion = NA))

set.seed(108727)

g1 <- mcmc_chico(1000000) |>
  as.tibble() |>
  mutate(t = 1:n()) |>
  ggplot(aes(t, value)) +
  geom_line() + sin_lineas + ylab(expression(theta)) +
  ylim(0, 0.5)

g2 <- tibble(x = rgamma(10000, 20, 100)) |>
  ggplot(aes(y = x, x = "")) +
  geom_violin() +
  ylab("") + sin_lineas +
  ylim(0, 0.5)

g1 + g2 + plot_layout(widths = c(5, 1))

## En mas dimensiones -------------------------------

library(mvtnorm)
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

mu <- c(1, 2)
Sigma <- matrix(c(1, .75, .75, 1), nrow = 2)
objetivo <- ModeloNormalMultivariado$new(mu, Sigma)


genera_experimento <- function(sigma){
  muestreo <- ModeloNormalMultivariado$new(c(0,0),
                                           sigma * diag(c(1,1)))
  set.seed(10)
  mcmc_multi <- crea_metropolis_hastings(objetivo, muestreo)
  mcmc_multi(50) |>
    as.tibble()
}

set.seed(108727)
## Para dibujar las curvas de nivel - distribucion objetivo 
plot.grid <- expand_grid(x = seq(-2,5, by = 7/99), y = seq(-1,5, by = 6/99))
plot.grid <- plot.grid %>% 
  mutate(density.target = dmvnorm(plot.grid, mean = mu, sigma = Sigma))
plot.breaks.target <- plot.grid %>% 
  summarise(breaks = quantile(density.target, probs = c(.67, .90, .99, 1))) %>% 
  pull(breaks)

## Caminatas aleatorias 
muestras.normal <- tibble(sigma = c(.1, .75, 2.33/sqrt(2), 5)) |>
   mutate(muestras = map(sigma, genera_experimento)) |>
   unnest(muestras)

## Para dibujar las curvas de nivel - distribucion propuesta
contours.proposal <- muestras.normal |>
  filter(sigma == 2.33/sqrt(2)) |>
  slice(1,3,7) |> mutate(id = 1:3) |>
  nest(location = c(V2, V3)) |>
  mutate(density.proposal = map(location,
         function(x){
           dmvnorm(plot.grid |> select(x,y),
                   mean = as.matrix(x),
                   sigma = 1.65 * diag(c(1,1)))
         }),
         coords = list(plot.grid |> select(x,y))) |>
  mutate(breaks.proposal = map(density.proposal, quantile, probs = c(.67,.90,.99)))

contours.proposal |>
  unnest(location, density.proposal, coords) |> 
  ggplot(aes(x, y, z = density.proposal)) +
  geom_contour_filled(bins = 4) + scale_fill_brewer(palette = "Purples") + 
  geom_point(data = contours.proposal |> unnest(location),
             aes(V2, V3), shape = 19, size = 10) +
  geom_contour(data = plot.grid, aes(x,y,z = density.target),
               breaks = plot.breaks.target, color = "black") +
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  facet_wrap(~id) + sin_lineas + coord_equal() + sin_leyenda

contours.proposal |>
  mutate(denominator = map(location, objetivo$density),
         numerator   = map(coords  , objetivo$density)) |>
  unnest(numerator, denominator) |>
  mutate(metropolis.hastings = ifelse(exp(numerator-denominator) < 1,
                                      exp(numerator-denominator), 1.00),
         contours.proposal |> unnest(coords) |> select(x,y),
         contours.proposal |> unnest(density.proposal),
         alpha = metropolis.hastings * density.proposal) |>
  ggplot(aes(x, y, z = log(metropolis.hastings + 1))) +
  geom_contour_filled(bins = 7) +
  scale_fill_brewer(palette = "Purples", direction = 1) +
  facet_wrap(~id) + sin_lineas + coord_equal() + sin_leyenda + 
  geom_point(data = contours.proposal |> unnest(location),
             aes(V2, V3), inherit.aes = FALSE, shape = 19, size = 10) +
  xlab(expression(x[1])) + ylab(expression(x[2]))

contours.proposal |>
  mutate(denominator = map(location, objetivo$density),
         numerator   = map(coords  , objetivo$density)) |>
  unnest(numerator, denominator) |>
  mutate(metropolis.hastings = ifelse(exp(numerator-denominator) < 1,
                                      exp(numerator-denominator), 1.00),
         contours.proposal |> unnest(coords) |> select(x,y),
         contours.proposal |> unnest(density.proposal),
         alpha = metropolis.hastings * density.proposal) |>
  ggplot(aes(x, y, z = alpha)) +
  geom_contour_filled(bins = 5) +
  scale_fill_brewer(palette = "Purples") +
  facet_wrap(~id) + sin_lineas + coord_equal() + sin_leyenda + 
  geom_point(data = contours.proposal |> unnest(location),
             aes(V2, V3), inherit.aes = FALSE, shape = 19, size = 10) +
  xlab(expression(x[1])) + ylab(expression(x[2]))

## Caminatas aleatorias 
muestras.normal |>
  ggplot(aes(x = V2, y = V3)) +
  geom_contour_filled(data = plot.grid, aes(x,y,z = density.target),
               breaks = plot.breaks.target) +
  scale_fill_brewer(palette = "Reds") + 
  geom_path() + geom_point() + 
  facet_wrap(~round(sigma,2), nrow = 1) + 
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + coord_equal() + sin_leyenda

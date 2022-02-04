## Setup --------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 15))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
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

islas <- tibble(islas = 1:5, pob = seq(60, 100, by = 10))
camina_isla <- function(i){ # i: isla actual
  u_izq <- runif(1) # Lanzamos volado para ver si nos vamos izq o der. 
  v <- ifelse(u_izq < 0.5, i - 1, i + 1)  # Pedimos índice isla vecina. 
  if (v < 1 | v > 5) { # si estas en los extremos y el volado indica salir
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
iteraciones[1] <- sample(1:5, 1) # isla inicial
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

## Animación histograma -----------------------------------
library(gganimate)
res <- caminata |>
  mutate(tiempo = cut(paso, breaks = seq(0, n(), by = 10))) |>
  group_by(isla, tiempo) |>
  count() |>
  ungroup() |>
  complete(tiempo, nesting(isla), fill = list(n = 0)) |>
  group_by(isla) |>
  mutate(count = cumsum(n)) |>
  group_by(tiempo) |>
  mutate(prop = count/sum(count)) |>
  arrange(tiempo, isla) |>
  ungroup()


anim <- res |>
  mutate(tiempo = as.numeric(tiempo)) |>
  filter(tiempo <= 1500) |>
  ggplot(aes(x = isla, y = prop)) +
  geom_bar(fill = "darkgray", stat = "identity") +
  coord_flip() + sin_lineas +
  geom_bar(data = islas |>  mutate(prop = pob/sum(pob)),
           aes(x = islas, y = prop), fill = "steelblue", alpha = .3, stat = "identity") + 
  scale_x_continuous(expression(theta), breaks = 1:10) +
  transition_states(tiempo, transition_length = 2, state_length = 1) +
  ease_aes("exponential-out")

animate(anim, renderer = ffmpeg_renderer(), height = 300, width = 900)

anim_save("./images/islas-histograma.mp4")

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
    muestras[1,2] <- estado
    muestras[1,1] <- 1
    for (ii in 2:niter){
      propuesta   <- estado + muestreo$sample()
      p_propuesta <- objetivo$density(propuesta, log = FALSE)
      p_estado    <- objetivo$density(estado, log = FALSE)
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
  ggtitle(paste("Trayectoria, eficiencia: ", mean(muestras[,2])))

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
               tasa.aceptacion = NA)) |>
  as.data.frame()

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

## Para dibujar las curvas de nivel 
plot.grid <- expand_grid(x = seq(-2,5, by = 7/99), y = seq(-1,5, by = 6/99))
plot.grid <- plot.grid %>% 
  mutate(density = dmvnorm(plot.grid, mean = mu, sigma = Sigma))
plot.breaks <- plot.grid %>% 
  summarise(breaks = quantile(density, probs = c(.67, .90, .99))) %>% 
  pull(breaks)

tibble(sigma = c(.1, .75, 2.33/sqrt(2), 5)) |>
  mutate(muestras = map(sigma, genera_experimento)) |>
  unnest(muestras) |>
  ggplot(aes(x = V2, y = V3)) +
  geom_contour(data = plot.grid, aes(x,y,z = density), breaks = plot.breaks) + 
  geom_path() + geom_point() + 
  facet_wrap(~round(sigma,2), nrow = 1) + 
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + coord_equal()

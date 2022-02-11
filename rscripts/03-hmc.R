## Setup --------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 20))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), axis.text = element_blank())

## Modelo normal -------------------------------
library(R6)
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
            },
            grad_log = function(x){
              -solve(self$cov, (x - self$mean))
            }
          ))

mu <- c(1, 2)
Sigma <- matrix(c(1, .75, .75, 1), nrow = 2)
objetivo <- ModeloNormalMultivariado$new(mu, Sigma)

set.seed(108727)
## Para dibujar las curvas de nivel - distribucion objetivo 
plot.grid <- expand_grid(x = seq(-2,5, by = 7/99), y = seq(-1,5, by = 6/99))
plot.grid <- plot.grid %>% 
  mutate(density.target = objetivo$density(plot.grid, log = FALSE))
plot.breaks.target <- plot.grid %>% 
  summarise(breaks = quantile(density.target, probs = c(.67, .90, .99, 1))) %>% 
  pull(breaks)


contours.proposal.mala <- tibble(id = 1:3,
       x = c(0.0241, -0.203, -0.59),
       y = c(-0.237, -0.0175, 1.25)) |>
  nest(location = c(x,y)) |>
  mutate(density.mala = map(location,
         function(x){
           ## Calcula el gradiente
           log.grad.objective <- objetivo$grad_log(as.matrix(x) |>
                                                   matrix(nrow = 2)) |>
             t()
           ## Define la distribucion propuesta
           tau <- 0.5
           propuesta <- ModeloNormalMultivariado$new(
                            mu =  as.matrix(x) + tau * log.grad.objective,
                            sigma = 2 * tau * diag(c(1,1))
                            )
           ## Evalua la distribucion propuesta en el grid
           propuesta$density(plot.grid |> select(x,y), log = FALSE)
         }),
         coords = list(plot.grid |> select(x,y)))

contours.proposal.mala |>
  unnest(density.mala, coords) |>
  ggplot(aes(x, y, z = density.mala)) +
  geom_contour_filled(bins = 4) + scale_fill_brewer(palette = "Purples") +
  geom_point(data = contours.proposal.mala |> unnest(location),
             aes(x, y), shape = 19, size = 10) +
  geom_contour(data = plot.grid, aes(x,y,z = density.target),
               breaks = plot.breaks.target, color = "black") +
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  facet_wrap(~id) + sin_lineas + coord_equal() + sin_leyenda

## Modelo normal con alta correlacion -------------------------
mu <- c(1, 2)
Sigma <- matrix(c(1, .90, .90, 1), nrow = 2)
objetivo <- ModeloNormalMultivariado$new(mu, Sigma)

## Para dibujar las curvas de nivel - distribucion objetivo 
plot.grid <- expand_grid(x = seq(-2,5, by = 7/99), y = seq(-1,5, by = 6/99))
plot.grid <- plot.grid %>% 
  mutate(density.target = objetivo$density(plot.grid, log = FALSE))
plot.breaks.target <- plot.grid %>% 
  summarise(breaks = quantile(density.target, probs = c(.67, .90, .99, 1))) %>% 
  pull(breaks)

## Para dibujar las curvas de nivel - distribucion propuesta
propuesta.std <- ModeloNormalMultivariado$new(c(1,2), diag(c(1,1)))

plot.grid.std <- expand_grid(x = seq(-2,5, by = 7/99), y = seq(-1,5, by = 6/99))
plot.grid.std <- plot.grid.std %>% 
  mutate(density.proposal = propuesta.std$density(plot.grid.std, log = FALSE))
plot.breaks.propuesta <- plot.grid.std %>% 
  summarise(breaks = quantile(density.proposal, probs = c(.67, .90, .99, 1))) %>% 
  pull(breaks)

plot.grid |>  
  ggplot(aes(x, y, z = density.target)) +
  geom_contour_filled(breaks = plot.breaks.target) +
  scale_fill_brewer(palette = "Reds") +
  geom_contour(data = plot.grid.std, aes(x,y,z = density.proposal),
               breaks = plot.breaks.propuesta, color = "black") +
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + coord_equal() + sin_leyenda

### Comparando muestras ----------------------

mu <- c(1, 2)
Sigma <- matrix(c(1, .90, .90, 1), nrow = 2)
objetivo <- ModeloNormalMultivariado$new(mu, Sigma)
propuesta.std <- ModeloNormalMultivariado$new(c(1,2), diag(c(1,1)))

muestras <- objetivo$sample(1000) |>
  rbind(propuesta.std$sample(1000)) |>
  as.tibble() |>
  mutate(tipo = rep(c("objetivo", "propuesta"), each = 1000))   

g1 <- muestras |>
  ggplot(aes(V1, V2)) +
  geom_point(aes(color = tipo)) +
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + coord_equal() + sin_leyenda +
  ggtitle("Diagrama de dispersión")

g2 <- muestras |>
  ggplot(aes(V1)) +
  geom_histogram(aes(fill = tipo), position = "identity", alpha = .6) +
  xlab(expression(x[1])) + 
  sin_lineas  + sin_leyenda +
    ggtitle("Histogramas")

g3 <- muestras |>
  ggplot(aes(V2)) +
  geom_histogram(aes(fill = tipo), position = "identity", alpha = .6) +
  xlab(expression(x[2])) + 
  sin_lineas + sin_leyenda

g1 + g2 + g3

plot.grid |>  
  ggplot(aes(x, y, z = density.target)) +
  geom_contour_filled(bins = 9) +
  scale_fill_brewer(palette = "Greys") +
  xlab(expression(x[1])) + ylab(expression(x[2])) + 
  sin_lineas + coord_equal() + sin_leyenda

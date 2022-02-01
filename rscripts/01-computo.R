## Setup --------------------------------------------------

library(tidyverse)
library(patchwork)
library(scales)
## Cambia el default del tamaño de fuente 
theme_set(theme_linedraw(base_size = 25))

## Cambia el número de decimales para mostrar
options(digits = 2)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
color.itam  <- c("#00362b","#004a3b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f", NA)

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), 
      axis.text = element_blank())

## Ejemplo de integracion numerica -----------------------

grid.n          <- 11                 # Número de celdas 
grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
norm.cuadrature <- tibble(x = seq(-3, 3, by = grid.size), y = dnorm(x) )


norm.density <- tibble(x = seq(-5, 5, by = .01), 
       y = dnorm(x) )

norm.cuadrature |>
  ggplot(aes(x=x + grid.size/2, y=y)) + 
  geom_area(data = norm.density, aes(x = x, y = y), fill = 'lightblue') + 
  geom_bar(stat="identity", alpha = .3) + 
  geom_bar(aes(x = x + grid.size/2, y = -0.01), fill = 'black', stat="identity") + 
  sin_lineas + xlab('x') + ylab("density") + 
  annotate('text', label = expression(Delta~u[n]),
           x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
  annotate('text', label = expression(f(u[n]) ),
           x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
  annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
           x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
           angle = -90, alpha = .7, size = 12)

grid.n          <- 101                 # Número de celdas 
grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
norm.cuadrature <- tibble(x = seq(-3, 3, by = grid.size), y = dnorm(x) )

norm.cuadrature |>
    ggplot(aes(x=x + grid.size/2, y=y)) + 
    geom_area(data = norm.density, aes(x = x, y = y), fill = 'lightblue') + 
    geom_bar(stat="identity", alpha = .3) + 
    geom_bar(aes(x = x + grid.size/2, y = -0.01), fill = 'black', stat="identity") + 
    sin_lineas + xlab('x') + ylab("density") + 
    annotate('text', label = expression(Delta~u[n]),
             x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
    annotate('text', label = expression(f(u[n]) ),
             x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
    annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
             x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
             angle = -90, alpha = .7, size = 12)

crear_log_post <- function(n, k){
  function(theta){
    verosim <- k * log(theta) + (n - k) * log(1 - theta)
    inicial <- log(theta)
    verosim + inicial
  }
}

# observamos 3 éxitos en 4 pruebas:
log_post <- crear_log_post(4, 3)
prob_post <- function(x) { exp(log_post(x))}
# integramos numéricamente
p_x <- integrate(prob_post, lower = 0, upper = 1, subdivisions = 100L)
p_x

media_funcion <- function(theta){
  theta * prob_post(theta) / p_x$value
}
integral_media <- integrate(media_funcion,
                            lower = 0, upper = 1,
                            subdivisions = 100L)
media_post <- integral_media$value 
c(Numerico = media_post, Analitico = 5/(2+5))

canvas <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
 xlim(0.5, 6) +
 ylim(40, 110)

grid.size <- 10 - 1

mesh <- expand.grid(x = seq(0.5, 6, by = (6-.5)/grid.size),
                    y = seq(40, 110, by = (110-40)/grid.size))

canvas + geom_density_2d_filled(aes(alpha = ..level..), bins = 8) + 
  scale_fill_manual(values = rev(color.itam)) + 
  sin_lineas + theme(legend.position = "none") +
  geom_point(data = mesh, aes(x = x, y = y)) + 
  annotate("rect", xmin = .5 + 5 * (6-.5)/grid.size, 
            xmax = .5 + 6 * (6-.5)/grid.size, 
            ymin = 40 + 3 * (110-40)/grid.size, 
            ymax = 40 + 4 * (110-40)/grid.size,
            linestyle = 'dashed', 
           fill = 'salmon', alpha = .4) + ylab("") + xlab("") + 
  annotate('text', x = .5 + 5.5 * (6-.5)/grid.size, 
                   y = 40 + 3.5 * (110-40)/grid.size, 
           label = expression(u[n]), color = 'red', size = 20) +
    theme(axis.ticks = element_blank(), 
        axis.text = element_blank())

## Integración Monte Carlo ----------------------------------- 
genera_dardos <- function(n = 100){
    tibble(x1 = runif(n, min = -1, max = 1), 
           x2 = runif(n, min = -1, max = 1)) %>% 
      mutate(resultado = ifelse(x1**2 + x2**2 <= 1., 1., 0.))
  }

  dardos <- tibble(n = seq(2,5)) %>% 
    mutate(datos = map(10**n, genera_dardos)) %>% 
    unnest() 

  dardos %>% 
    ggplot(aes(x = x1, y = x2)) + 
      geom_point(aes(color = factor(resultado))) + 
      facet_wrap(~n, nrow = 1) +  
    sin_lineas + sin_ejes + sin_leyenda

dardos |>
  group_by(n) |>
  summarise(aprox = 4 * mean(resultado)) |>
  as.data.frame()

set.seed(1087)

genera_dardos(n = 2**16) %>% 
  mutate(n = seq(1, 2**16), 
         approx = cummean(resultado) * 4) %>% 
  ggplot(aes(x = n, y = approx)) + 
    geom_line() + 
    geom_hline(yintercept = pi, linetype = 'dashed') + 
    scale_x_continuous(trans='log10', 
                       labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Muestras") + sin_lineas

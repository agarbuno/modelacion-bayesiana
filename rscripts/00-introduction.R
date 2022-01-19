library(tidyverse)
library(patchwork)
library(scales)

sin_lineas <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

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
           x = .01 + 5 * grid.size/2, y = -.02) + 
  annotate('text', label = expression(f(u[n]) * p(u[n]) ),
           x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)) + 
  annotate('text', label = expression(f(u[n]) * p(u[n]) * Delta~u[n]), 
           x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
           angle = -90, alpha = .7)

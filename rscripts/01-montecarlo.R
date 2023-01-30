## Setup --------------------------------------------------

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

distancia_euclideana <- function(u) sqrt(sum(u * u));

experimento <- function(ndim){
  nsamples <- 1e5; 
  y <- matrix(runif(nsamples * ndim, -0.5, 0.5), nsamples, ndim);
  mean(apply(y, 1, distancia_euclideana) < 0.5)
}

tibble(dims = 1:10) |>
  mutate(prob = map_dbl(dims, experimento)) |>
  ggplot(aes(dims, prob)) +
  geom_point() +
  geom_line() +
  sin_lineas +
  scale_x_continuous(breaks=c(1, 3, 5, 7, 9)) +
xlab("Número de dimensiones") +
ylab("Volumen relativo")

chi.pdf <- function(x, d) {
  x^(d - 1) * exp(-x^2/2) / (2^(d/2 - 1) * gamma(d/2))
}

puntos.grafica <- tibble(dims = c(1, 5, 10, 25, 50, 100)) |>
  mutate(points = map(dims, function(dim){
    tibble(x = seq(0, 15, length.out = 1000)) |>
      mutate(y = chi.pdf(x, dim))
  }), dimensions = factor(dims))

puntos.grafica |>
  unnest(points) |>
  ggplot(aes(x, y, group = dimensions, color = dimensions)) +
  geom_line() + sin_leyenda + 
  sin_lineas + xlab("Número de dimensiones") +
  ylab("Densidad")

tibble(dim = 2**seq(0, 8)) |>
  mutate(.centro = sqrt(qchisq(.50, dim)),
         .lower = sqrt(qchisq(.025, dim)),
         .upper = sqrt(qchisq(.975, dim))) |>
ggplot(aes(dim, .centro)) +
geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .3, fill = "gray") + 
geom_line() + geom_point() + sin_lineas +
scale_x_log10() +
ylab("Distancia euclideana al centro") +
xlab("Número de dimensiones")

tibble(dim = 2**seq(0, 8)) |>
  mutate(.resultados  = map(dim, function(ndim){
           x <- unlist(purrr::rerun(10000, sum(dnorm(rnorm(ndim),log = TRUE))))
           tibble(x = x) |>
             summarise(.densidad_tip = mean(x),
                       .lower_densidad = quantile(x, .025),
                       .upper_densidad = quantile(x, .975),
                       .densidad_moda = sum(dnorm(rep(0, ndim), log = TRUE)))
         })) |>
  unnest(.resultados) |>
  ggplot(aes(dim, .densidad_tip)) +
  geom_line(aes(y = .densidad_moda), col = 'red') +
  geom_point(aes(y = .densidad_moda), col = 'red') + 
  geom_ribbon(aes(ymin = .lower_densidad, ymax = .upper_densidad), alpha = .3, fill = "gray") + 
  geom_line() + geom_point() + sin_lineas +
  scale_x_log10() +
  ylab("log-Densidad") +
  xlab("Número de dimensiones")

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
  sin_lineas + xlab('') + ylab("") + 
  annotate('text', label = expression(Delta~u[n]),
           x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
  annotate('text', label = expression(f(u[n]) ),
           x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
  annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
           x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
           angle = -90, alpha = .7, size = 12) + sin_ejes

grid.n          <- 101                 # Número de celdas 
grid.size       <- 6/(grid.n+1)       # Tamaño de celdas en el intervalo [-3, 3]
norm.cuadrature <- tibble(x = seq(-3, 3, by = grid.size), y = dnorm(x) )

norm.cuadrature |>
    ggplot(aes(x=x + grid.size/2, y=y)) + 
    geom_area(data = norm.density, aes(x = x, y = y), fill = 'lightblue') + 
    geom_bar(stat="identity", alpha = .3) + 
    geom_bar(aes(x = x + grid.size/2, y = -0.01), fill = 'black', stat="identity") + 
    sin_lineas + xlab('') + ylab("") + 
    annotate('text', label = expression(Delta~u[n]),
             x = .01 + 5 * grid.size/2, y = -.02, size = 12) + 
    annotate('text', label = expression(f(u[n]) ),
             x = .01 + 9 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2), size = 12) + 
    annotate('text', label = expression(f(u[n]) * Delta~u[n]), 
             x = .01 + 5 * grid.size/2, y = dnorm(.01 + 4 * grid.size/2)/2, 
             angle = -90, alpha = .7, size = 12) + sin_ejes

crear_log_post <- function(n, k){
  function(theta){
    verosim <- k * log(theta) + (n - k) * log(1 - theta)
    inicial <- log(theta)
    verosim + inicial
  }
}

# observamos 3 exitos en 4 pruebas:
log_post <- crear_log_post(4, 3)
prob_post <- function(x) { exp(log_post(x))}
# integramos numericamente
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

g1 <- canvas +
  geom_density_2d_filled(aes(alpha = ..level..), bins = 8) +
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
           label = expression(u[n]), color = 'red', size = 15) +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank())


g2 <- canvas + 
  stat_bin2d(aes(fill = after_stat(density)), binwidth = c((6-.5)/grid.size, (110-40)/grid.size)) +
  sin_lineas + theme(legend.position = "none") +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  scale_fill_distiller(palette = "Greens", direction = 1) + 
  sin_lineas + theme(legend.position = "none") +
  ylab("") + xlab("")

g3 <- canvas + 
  stat_bin2d(aes(fill = after_stat(density)), binwidth = c((6-.5)/25, (110-40)/25)) +
  sin_lineas + theme(legend.position = "none") +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  scale_fill_distiller(palette = "Greens", direction = 1) + 
  sin_lineas + theme(legend.position = "none") +
  ylab("") + xlab("")

g1 + g2 + g3

## Integración Monte Carlo ----------------------------------- 
genera_dardos <- function(n = 100){
    tibble(x1 = runif(n, min = -1, max = 1), 
           x2 = runif(n, min = -1, max = 1)) |> 
      mutate(resultado = ifelse(x1**2 + x2**2 <= 1., 1., 0.))
  }

  dardos <- tibble(n = seq(2,5)) |> 
    mutate(datos = map(10**n, genera_dardos)) |> 
    unnest() 

  dardos |> 
    ggplot(aes(x = x1, y = x2)) + 
      geom_point(aes(color = factor(resultado))) + 
      facet_wrap(~n, nrow = 1) +  
    sin_lineas + sin_ejes + sin_leyenda + coord_equal()

dardos |>
  group_by(n) |>
  summarise(aprox = 4 * mean(resultado))

set.seed(1087)
genera_dardos(n = 2**16) |> 
  mutate(n = seq(1, 2**16), 
         approx = cummean(resultado) * 4) |> 
  ggplot(aes(x = n, y = approx)) + 
    geom_line() + 
    geom_hline(yintercept = pi, linetype = 'dashed') + 
    scale_x_continuous(trans='log10', 
                       labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas

set.seed(108)
nsamples <- 10**4; nexp <- 100
U <- runif(nexp * 2 * nsamples)
U <- array(U, dim = c(nexp, 2, nsamples))
apply(U[1:5,,], 1, str)

resultados <- apply(U, 1, function(x){
  dardos <- apply(x**2, 2, sum)
  exitos <- ifelse(dardos <= 1, 1, 0)
  prop   <- cummean(exitos)
  4 * prop
})

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:10) |>
  ggplot(aes(n, value)) +
  geom_line(aes(group = name, color = name)) +
  geom_hline(yintercept = pi, linetype = 'dashed') + 
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda +
  ylim(0, 7)

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:nexp) |>
  group_by(n) |>
  summarise(promedio = mean(value),
            desv.est = sd(value),
            y.lo = promedio - 2 * desv.est,
            y.hi = promedio + 2 * desv.est) |>
  ggplot(aes(n , promedio)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), fill = "gray", alpha = .3) +
  geom_ribbon(aes(ymin = promedio - 2 * sqrt(pi * (4 - pi)/(n)),
                  ymax = promedio + 2 * sqrt(pi * (4 - pi)/(n))),
              fill = "salmon", alpha = .1) +
  geom_hline(yintercept = pi, linetype = 'dashed') + 
  geom_line() +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Aproximación') + xlab("Número de muestras") + sin_lineas + sin_leyenda +
ylim(0, 7)

resultados |>
  as_data_frame() |>
  mutate(n = 1:nsamples) |>
  pivot_longer(cols = 1:nexp) |>
  group_by(n) |>
  summarise(varianza = var(value/4)) |>
  mutate(cramer.rao = pi * (4 - pi)/(16 * n)) |>
  ggplot(aes(n , varianza)) +
  geom_line() +
  geom_line(aes(n, cramer.rao), lty = 2, color = 'red') +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10', 
                     labels = trans_format("log10", math_format(10^.x))) + 
  ylab('Varianza') + xlab("Número de muestras") + sin_lineas + sin_leyenda

### Ejemplo proporciones ------------------

theta <- rbeta(10000, 5, 2)
media_post <- mean(theta)
momento_2_post <- mean(theta^2)
c(mu_1 = media_post, mu_2 = momento_2_post)

mean(exp(theta) > 2)

### Ejemplo helados -------------------------

datos <- tibble(
  sabor = c("fresa", "limon", "mango", "guanabana"),
  n = c(50, 45, 51, 50), gusto = c(36, 35, 42, 29)) |> 
  mutate(prop_gust = gusto / n)

datos

datos <- datos |>
  mutate(a_post = gusto + 2,
         b_post = n - gusto + 1,
         media_post = a_post/(a_post + b_post))
datos

modelo_beta <- function(params, n = 5000){
  rbeta(n, params$alpha, params$beta)
}

## Generamos muestras de la posterior
paletas <- datos |>
  mutate(alpha = a_post, beta = b_post) |>
  nest(params.posterior = c(alpha, beta)) |>
  mutate(muestras.posterior = map(params.posterior, modelo_beta)) |>
  select(sabor, muestras.posterior)

paletas |>
  unnest(muestras.posterior) |>
  ggplot(aes(muestras.posterior)) +
  geom_histogram(aes(fill = sabor), position = "identity", alpha = .8) +
  sin_lineas

## Utilizamos el metodo Monte Carlo para aproximar la integral. 
paletas |>
  unnest(muestras.posterior) |>
  mutate(id = rep(seq(1, 5000), 4)) |> group_by(id) |>
  summarise(favorito = sabor[which.max(muestras.posterior)]) |>
  group_by(favorito) |> tally() |>
  mutate(prop = n/sum(n))

crea_mezcla <- function(weights){
  function(x){
    weights$w1 * dnorm(x, mean = -1.5, sd = .5) +
      weights$w2 * dnorm(x, mean = 1.5, sd = .7)
  }
}
objetivo <- crea_mezcla(list(w1 = .6, w2 = .4))

muestras_mezcla <- function(id){
  n <- 100
  tibble(u = runif(n)) |>
    mutate(muestras = ifelse(u <= .6,
                             rnorm(1, -1.5, sd = .5),
                             rnorm(1,  1.5, sd = .7))) |>
    pull(muestras)
}

muestras.mezcla <- tibble(id = 1:1000) |>
  mutate(muestras  = map(id, muestras_mezcla)) |>
  unnest(muestras) |>
  group_by(id) |>
  summarise(estimate = mean(muestras))

g0 <- muestras.mezcla |>
  ggplot(aes(estimate)) +
  geom_histogram() +
  geom_vline(xintercept = -1.5 * .6 + 1.5 * .4,
             lty = 2, color = 'salmon', lwd = 1.5) +
  geom_vline(xintercept = mean(muestras.mezcla$estimate),
             lty = 3, color = 'steelblue', lwd = 1.5) +
  xlim(-1, 1) +
  ggtitle("Objetivo") + sin_lineas

muestras_uniforme <- function(id){
  n <- 100
  runif(n, -5, 5)
}

muestras.uniforme <- tibble(id = 1:1000) |>
  mutate(muestras  = map(id, muestras_uniforme)) |>
  unnest(muestras) |>
  mutate(pix = objetivo(muestras),
         gx  = dunif(muestras, -5, 5),
         wx  = pix/gx) |>
  group_by(id) |>
  summarise(estimate = sum(muestras * wx)/sum(wx))

g1 <- muestras.uniforme |>
  ggplot(aes(estimate)) +
  geom_histogram() +
  geom_vline(xintercept = -1.5 * .6 + 1.5 * .4,
             lty = 2, color = 'salmon', lwd = 1.5) +
  geom_vline(xintercept = mean(muestras.uniforme$estimate),
             lty = 3, color = 'steelblue', lwd = 1.5) +
  xlim(-1, 1) +
  ggtitle("Uniforme(-5,5)") + sin_lineas

muestras_importancia <- function(id){
  n <- 100
  rnorm(n, 0, sd = 1)
}  

muestras.normal  <- tibble(id = 1:1000) |>
  mutate(muestras  = map(id, muestras_importancia)) |>
  unnest(muestras) |>
  mutate(pix = objetivo(muestras),
         gx  = dnorm(muestras, 0, sd = 1),
         wx  = pix/gx) |>
  group_by(id) |>
  summarise(estimate = sum(muestras * wx)/sum(wx))

g2  <- muestras.normal |> ggplot(aes(estimate)) +
  geom_histogram() +
  geom_vline(xintercept = -1.5 * .6 + 1.5 * .4,
             lty = 2, color = 'salmon', lwd = 1.5) +
  geom_vline(xintercept = mean(muestras.normal$estimate),
             lty = 3, color = 'steelblue', lwd = 1.5) +
  xlim(-1, 1) +
  ggtitle("Normal(0, 2)") +
  sin_lineas

g0 + g1 + g2

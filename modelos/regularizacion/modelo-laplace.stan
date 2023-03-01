data {
  int<lower=0>  p;       // Numero de parametros
  real<lower=0> sigma;   // Varianza en la previa
}

parameters {
  vector[p] theta;
}

model {
  theta ~ double_exponential(0, sigma);
}

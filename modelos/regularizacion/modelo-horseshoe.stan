data {
  int<lower=0>  p;                      // Numero de parametros
  real<lower=0> sigma;                  // Varianza en la previa
}

parameters {
  vector[p] theta_tilde;
  vector<lower=0>[p] lambda;
  real<lower=0> tau_tilde;
}

model {
  theta_tilde ~ normal(0, 1);
  tau_tilde   ~ cauchy(0, 1);
  lambda      ~ cauchy(0, 1);
}

generated quantities{
  vector[p] theta = theta_tilde .* lambda * sigma * tau_tilde; 
}

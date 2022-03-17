data {
  // Datos
  int N;
  real y[N];
  // Hiper-parametros previa
  real mu0;
  real<lower=0> n0;
  real<lower=0> a0;
  real<lower=0> b0;
}

parameters {
  real mu_tilde;
  real<lower=0> tau;
}

transformed parameters {
  real sigma = 1/tau;
  real mu  = mu0 + (sigma/n0) * mu_tilde;
}

model {
  y ~ normal(mu, sigma);
  mu_tilde ~ normal(0, 1);
  tau ~ gamma(a0, b0);
}

generated quantities {
  real y_tilde = normal_rng(mu, sigma);
}

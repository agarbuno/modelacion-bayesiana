data {
  real mu0;
  real<lower=0> n0;
  real<lower=0> a0;
  real<lower=0> b0;
}
generated quantities {
  real tau = gamma_rng(a0, b0);
  real sigma = 1/tau; 
  real mu  = normal_rng(mu0, sigma/n0);
  real y_tilde = normal_rng(mu, sigma);
}

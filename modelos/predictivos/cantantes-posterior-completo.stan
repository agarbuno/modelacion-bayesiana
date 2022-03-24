data {
  int N;
  real y[N]; 
  real mu0;
  real<lower=0> n0;
  real<lower=0> a0;
  real<lower=0> b0;
}
parameters {
  real<lower=0> tau;
  real mu;
}
transformed parameters {
  real sigma = 1/tau; 
}
model {
  tau ~ gamma(a0, b0);
  mu  ~ normal(mu0, sigma/sqrt(n0));
  y   ~ normal(mu, sigma); 
}
generated quantities {
  array[N] real y_tilde = normal_rng(rep_array(mu, N), rep_array(sigma, N));
  real mean_y_tilde = mean(to_vector(y_tilde));
  real sd_y_tilde = sd(to_vector(y_tilde)); 
}

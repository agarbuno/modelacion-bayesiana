data {
  int N;
  int y[N];
  real<lower=0> gamma_alpha;
  real<lower=0> gamma_beta;
}

parameters {
  real log_lambda;
  real<lower=0> phi; 
}

model {
  log_lambda ~ normal(4, 0.5);
  phi ~ gamma(gamma_alpha, gamma_beta);
  y   ~ neg_binomial_2_log(log_lambda, phi);
}

generated quantities {
  int y_tilde = neg_binomial_2_log_rng(log_lambda, phi); 
}

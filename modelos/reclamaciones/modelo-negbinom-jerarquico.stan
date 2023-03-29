data {
  int N;
  int y[N];
  int compania[N];
  real<lower=0> gamma_alpha;
  real<lower=0> gamma_beta;
}

parameters {
  real log_lambda[6];
  real<lower=0> phi[6]; 
}

model {
  log_lambda ~ normal(4, 0.5);
  phi    ~ gamma(gamma_alpha, gamma_beta); 
  y      ~ neg_binomial_2_log(log_lambda[compania], phi[compania]);
}

generated quantities {
  int y_tilde[6];
  for (ii in 1:6){
    y_tilde[ii] = neg_binomial_2_log_rng(log_lambda[ii], phi[ii]);
  }
}

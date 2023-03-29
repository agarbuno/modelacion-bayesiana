data {
  int N;
  int y[N];
}

parameters {
  real<lower=0> lambda;
  real<lower=0> phi; 
}

model {
  lambda ~ normal(50, 10);
  phi    ~ gamma(1, 1); 
  y      ~ neg_binomial_2(lambda, phi);
}

generated quantities {
  int y_tilde = neg_binomial_2_rng(lambda, phi); 
}

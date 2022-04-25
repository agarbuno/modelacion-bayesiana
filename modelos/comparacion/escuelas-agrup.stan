data {
  int<lower=0> J;
  real y[J];
  real<lower=0> sigma[J];
}
parameters {
  real mu;
  real<lower=0> tau;
  real theta_tilde;
}
transformed parameters {
  real theta = mu + tau * theta_tilde; 
}
model {
  mu ~ normal(0, 5);
  tau ~ cauchy(0, 5);
  theta_tilde ~ normal(0, 1);
  y ~ normal(theta, sigma);
}
generated quantities {
  array[J] real log_lik;
  for (jj in 1:J){
    log_lik[jj] = normal_lpdf(y[jj] | theta, sigma[jj]);
  }
}

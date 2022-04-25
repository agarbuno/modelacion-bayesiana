data {
  int<lower=0> J;
  real y[J];
  real<lower=0> sigma[J];
}
parameters {
  real theta[J];
}
model {
  y ~ normal(theta, sigma);
}
generated quantities {
  array[J] real log_lik;
  for (jj in 1:J){
    log_lik[jj] = normal_lpdf(y[jj] | theta[jj], sigma[jj]);
  }
}

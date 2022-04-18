data {
  int<lower=0> N;
  int y[N];
}

parameters {
  ordered[2] mu;
  real<lower=0, upper=1> omega;
}

model {
  for(n in 1:N) {
    target += log_mix(omega,
                      poisson_log_lpmf(y[n] | mu[1]),
                      poisson_log_lpmf(y[n] | mu[2]));
  }
  target += normal_lpdf(mu | 3, 1);
}

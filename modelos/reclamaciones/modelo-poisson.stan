data {
  int N;
  int y[N];
}
parameters {
  real<lower=0> lambda; 
}
model {
  lambda ~ gamma(100, 2);
  y ~ poisson(lambda);
}

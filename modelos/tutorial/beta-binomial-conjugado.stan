data {
  int<lower = 0, upper = 10> Y;
}
generated quantities {
  real<lower=0, upper=1> theta;
  theta = beta_rng(Y + 2, 10 - Y + 2);
}

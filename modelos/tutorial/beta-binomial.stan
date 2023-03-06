data {
  int<lower = 0, upper = 10> Y;
}
parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  Y ~ binomial(10, theta);
  theta ~ beta(2, 2);
}

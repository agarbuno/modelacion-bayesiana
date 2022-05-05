data {
  int<lower=1> K;
  array[K] int<lower=0> N;
  array[K] int<lower=0> y;
}
parameters {
  vector<lower=0, upper=1>[K] theta;
}
model {
  y ~ binomial(N, theta);
}
generated quantities {
  array[K] int<lower=0, upper=1> mejor_ix;
  {
    real max_prob = max(theta);
    for (k in 1:K) {
      mejor_ix[k] = theta[k] >= max_prob;
    }
  }
}

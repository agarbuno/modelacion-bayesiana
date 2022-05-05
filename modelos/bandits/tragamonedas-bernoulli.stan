data {
  int<lower=1> K; 
  int<lower=0> N; 
  array[N] int<lower=1, upper=K> z; 
  array[N] int<lower=0, upper=1> y; 
}
parameters {
  vector<lower=0, upper=1>[K] theta;
}
model {
  theta ~ beta(1, 1); 
  y ~ bernoulli(theta[z]); 
}
generated quantities {
  simplex[K] mejor_ix; 
  {
    real mejor_prob = max(theta);
    for (k in 1 : K) {
      mejor_ix[k] = theta[k] >= mejor_prob;
    }
    mejor_ix /= sum(mejor_ix); 
  }
}

data {
  int<lower=1> K; 
  int<lower=0> N; 
  array[N] int<lower=1, upper=K> z; 
  array[N] int<lower=0, upper=1> y; 
}
transformed data {
  int<lower = 0> experimentos[K] = rep_array(0, K);
  int<lower = 0> exitos[K] = rep_array(0, K);
  for (n in 1:N) {
    experimentos[z[n]] += 1;
    exitos[z[n]] += y[n];
  }
}
parameters {
  vector<lower=0, upper=1>[K] theta;
}
model {
  theta ~ beta(1, 1);
  exitos ~ binomial(experimentos, theta);
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

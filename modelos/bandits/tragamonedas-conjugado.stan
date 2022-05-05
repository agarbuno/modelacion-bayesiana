data {
  int<lower=1> K; 
  int<lower=0> N; 
  array[N] int<lower=1, upper=K> z; 
  array[N] int<lower=0, upper=1> y; 
}
transformed data {
  array[K] int<lower = 0> experimentos = rep_array(0, K);
  array[K] int<lower = 0> exitos = rep_array(0, K);
  for (n in 1:N) {
    experimentos[z[n]] += 1;
    exitos[z[n]] += y[n];
  }
}
generated quantities {
  array[K] real<lower = 0, upper = 1> theta;
  for (k in 1:K)
    theta[k] = beta_rng(1 + exitos[k], 1 + experimentos[k] - exitos[k]);

  simplex[K] mejor_ix; 
  {
    real mejor_prob = max(theta);
    for (k in 1 : K) {
      mejor_ix[k] = theta[k] >= mejor_prob;
      }
      mejor_ix /= sum(mejor_ix); 
    }
  }

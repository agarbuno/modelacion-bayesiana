functions {
  real quadraticUtility(int y_tilde, int d) {
    return -(y_tilde - d)^2; 
  }
  real zeroOneUtility(int y_tilde, int d){
    if (y_tilde == d) {
      return 1;
    } else {
      return 0;
    } 
  }
  real intervalUtility(int y_tilde, int d){
    if (fabs(y_tilde - d) < 10) {
      return 0;
    } else {
      return -fabs(y_tilde - d); 
    }
  }
}

data {
  int<lower=0> N;
  int<lower=0> K;
  array[N] int<lower=0, upper=1> test;
}

parameters {
  real<lower=0, upper=1> theta; 
}

model {
  theta ~ beta(5, 11); 
  test ~ bernoulli(theta);
}

generated quantities {
  array[K] real utilQuad;
  array[K] real utilZeroOne;
  array[K] real utilInterval; 
  for (kk in 1:K) {
    utilQuad[kk] = quadraticUtility(binomial_rng(K, theta), kk);
    utilZeroOne[kk] = zeroOneUtility(binomial_rng(K, theta), kk);
    utilInterval[kk] = intervalUtility(binomial_rng(K, theta), kk); 
  }
}

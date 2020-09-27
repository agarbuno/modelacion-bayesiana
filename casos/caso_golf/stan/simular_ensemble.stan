functions {
  real prob(real x, real sigma) {
    return 2 * normal_cdf(atan(3.25 / x), 0, sigma) - 1;
  }
}

data {
  int p; // Número de cubetas
  int x[p]; // Distancia para cubeta
  int n[p]; // Número de intentos en cada cubeta
  real gamma_sigma[2];
}



generated quantities {
  real min_sigma = 0; // sigma mínima
  real<lower=0> sigma = gamma_rng(gamma_sigma[1], gamma_sigma[2]);
  int exitos[p];
  for(i in 1:p){
    exitos[i] = binomial_rng(n[i], prob(x[i], min_sigma + sigma));
  }
}


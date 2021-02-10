functions {
  real prob(real x, real sigma) {
    return 2 * normal_cdf(atan(3.25 / x), 0, sigma) - 1;
  }
}

data {
  int p; // Número de cubetas
  int x[p]; // Distancia para cubeta
  int n[p]; // Número de intentos en cada cubeta
  int exitos_obs[p]; // Número de exitos en cada cubeta
  int gamma_params[2];
}

parameters {
  real<lower=0> sigma;
}

model {
  sigma ~ gamma(gamma_params[1], gamma_params[2]);
  for(i in 1:p){
    exitos_obs[i] ~ binomial(n[i], prob(x[i], sigma));  
  }
}

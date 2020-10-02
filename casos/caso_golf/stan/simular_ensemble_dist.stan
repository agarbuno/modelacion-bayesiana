functions {
  real prob_angulo(real x, real sigma) {
    return 2 * normal_cdf(atan(3.25 / x), 0, sigma) - 1;
  }
  real prob_dist(real x, real sigma, real rho){
    return 2 * normal_cdf(0.82 / ( 0.82 + 3.26 *(rho) * x), 0, sigma) - 1;
  }
}

data {
  int p; // Número de cubetas
  int x[p]; // Distancia para cubeta
  int n[p]; // Número de intentos en cada cubeta
  real gamma_sigma_ang[2];
  real gamma_sigma_dist[2];
}



generated quantities {
  real<lower=0> sigma_angulo = gamma_rng(gamma_sigma_ang[1], gamma_sigma_ang[2]);
  real<lower=0> sigma_dist = gamma_rng(gamma_sigma_dist[1], gamma_sigma_dist[2]);
  real<lower=0, upper=1> rho = beta_rng(50, 50*(0.87/0.13));
  real prob;
  int exitos[p];
  for(i in 1:p){
    prob = prob_angulo(x[i], sigma_angulo) * prob_dist(x[i], sigma_dist, rho);
    exitos[i] = binomial_rng(n[i], prob);
  }
}


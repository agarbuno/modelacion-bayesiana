functions {
  real prob_ang(real x, real sigma) {
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
  real gamma_ang_pars[2];
  real gamma_dist_pars[2];
}

generated quantities {
  real<lower=0, upper=1> prob_exito[p];
  int exitos[p];
  real sigma_ang = gamma_rng(gamma_ang_pars[1], gamma_ang_pars[2]);
  real sigma_dist = gamma_rng(gamma_dist_pars[1], gamma_dist_pars[2]);

  for(i in 1:p){
    prob_exito[i] = prob_ang(x[i], sigma_ang) * prob_dist(x[i], sigma_dist, 0.131); 
    exitos[i] = binomial_rng(n[i], prob_exito[i]);
  }
}
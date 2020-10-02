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
  int exitos_obs[p]; // Número de exitos en cada cubeta
  real gamma_params_ang[2];
  real gamma_params_dist[2];
}

parameters {
  real<lower=0> sigma_angulo;
  real<lower=0> sigma_dist;
  real<lower=0, upper=1> rho;
}
transformed parameters {
  real<lower=0,upper=1> prob_exito[p];
    for(i in 1:p){
      prob_exito[i] = prob_angulo(x[i], sigma_angulo) * prob_dist(x[i], sigma_dist, rho);  
  }
  
}


model {
  rho ~ beta(50, 50*(0.87/0.13));
  sigma_angulo ~ gamma(gamma_params_ang[1], gamma_params_ang[2]);
  sigma_dist ~ gamma(gamma_params_dist[1], gamma_params_dist[2]);
  for(i in 1:p){
    exitos_obs[i] ~ binomial(n[i],  prob_exito[i]);  
  }
}

generated quantities {
  real prob_sim[p];
  for(i in 1:p){
    prob_sim[i] = binomial_rng(n[i], prob_exito[i]);
    prob_sim[i] = prob_sim[i] / n[i];
  }
}

  
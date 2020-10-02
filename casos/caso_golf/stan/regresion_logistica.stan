data {
  int p; // Número de cubetas
  int x[p]; // Distancia para cubeta
  int n[p]; // Número de intentos en cada cubeta
  int exitos_obs[p]; // Número de exitos en cada cubeta

}

parameters {
  real<lower=3, upper=6> beta_0;
  real<lower=0.007, upper=0.02> beta; 
}

transformed parameters {
  real<lower=0,upper=1> prob_exito[p];
    for(i in 1:p){
      prob_exito[i] = inv_logit(beta_0 - beta * x[i]);  
  }
}


model {
  beta_0 ~  uniform(3.0, 6.0);
  beta   ~ uniform(0.007, 0.02);
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

  
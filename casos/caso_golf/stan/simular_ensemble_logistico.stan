data {
  int p; // Número de cubetas
  int x[p]; // Distancia para cubeta
  int n[p]; // Número de intentos en cada cubeta
}



generated quantities {
  real beta_0 = uniform_rng(3.0, 6.0);
  real beta = uniform_rng(0.007, 0.02);
  int exitos[p];
  for(i in 1:p){
    exitos[i] = binomial_rng(n[i], inv_logit(beta_0 - beta * x[i]));
  }
}


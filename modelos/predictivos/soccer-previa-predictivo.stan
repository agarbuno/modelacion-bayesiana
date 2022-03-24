data {
  int<lower=0> J;
  array[2] real<lower=0> epsilon; 
}

generated quantities {
  array[J] real<lower=0> lambda;
  array[J, J] int y;
  // Generamos las lambdas 
  for (j in 1:J) lambda[j] = gamma_rng(epsilon[1], epsilon[2]);
  // Generamos de la predictiva 
  for (i in 1:J) {
    for (j in 1:J) {
      y[i, j] = poisson_rng(lambda[i]) - poisson_rng(lambda[j]);
    }
  }  
}

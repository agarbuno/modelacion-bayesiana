data {
  int N;
  real y[N]; 
}
parameters {
  real<lower=0> sigma;
  real mu;
}
model {
  y   ~ normal(mu, sigma); 
}
generated quantities {
  array[N] real y_tilde = normal_rng(rep_array(mu, N), rep_array(sigma, N));
  real mean_y_tilde = mean(to_vector(y_tilde));
  real sd_y_tilde = sd(to_vector(y_tilde)); 
}

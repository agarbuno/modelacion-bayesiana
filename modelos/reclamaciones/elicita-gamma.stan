functions {
  // Diferencias para las colas de una Gamma
  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;
    deltas[1] = gamma_cdf(theta[1] | exp(y[1]), exp(y[2])) - 0.005;
    deltas[2] = 1 - gamma_cdf(theta[2] | exp(y[1]), exp(y[2])) - 0.005;
    return deltas;
  }
}
transformed data {
  vector[2] y_guess = [log(9), log(0.5)]';//Valores iniciales
  vector[2] theta = [1.5, 15]';           //Cotas del intervalo
  vector[2] y;
  real x_r[0];
  int x_i[0];
  // Encuentra los parametros de la Gamma para satisfacer que
  // con 1% de probabilidad estemos en el intervalo [0.5, 20]
  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);

  print("alpha = ", exp(y[1]));
  print("beta = ", exp(y[2]));
}
generated quantities {
  real alpha = exp(y[1]);
  real beta = exp(y[2]);
}

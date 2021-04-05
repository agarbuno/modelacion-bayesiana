data{
    real<lower = 0> sigma_0;   
    int N;
    real y[N];
    vector[N] x;
}
parameters {
    real a; 
    real b; 
    real<lower = 0> sigma;
}
model {
    b ~ normal(50, 5);
    a ~ normal(0, 2);
    y ~ normal(a*x + b, sigma);
}
generated quantities{
    real y_new;
    y_new = normal_rng(a * 2 + b, sigma); 
}

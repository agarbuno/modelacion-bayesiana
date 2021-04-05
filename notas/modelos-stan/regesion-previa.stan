parameters {
    real a; 
    real b; 
}
model {
    b ~ normal(50, 5);
    a ~ normal(0, 2);
}

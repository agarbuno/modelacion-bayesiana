data {
    int J;
    int n[J];
    vector[J] x;
    int y[J];
    real r;
    real R;
    real overshot;
    real distance_tolerance;
}
transformed data {
    vector[J] threshold_angle = atan((R-r) ./ x);
    vector[J] raw_proportion  = to_vector(y) ./ to_vector(n);
}
parameters {
    real<lower=0> sigma_angle;
    real<lower=0> sigma_force;
    real<lower=0> sigma_obs;
}
transformed parameters {
    vector[J] p_angle = 2*Phi(threshold_angle / sigma_angle) - 1;
    vector[J] p_force = Phi((distance_tolerance - overshot) ./ ((x + overshot)*sigma_force)) -
               Phi((- overshot) ./ ((x + overshot)*sigma_force));
    vector[J] p = p_angle .* p_force;
}
model {
    raw_proportion ~ normal(p, sqrt(p .* (1-p) ./ to_vector(n) + sigma_obs^2));
}
generated quantities {
    real sigma_degrees = sigma_angle * 180 / pi();
    vector[J] residual = raw_proportion - p;
}
// Declare data
data {
  #include include/shared_data.stan
  matrix[ntime, nrec] delta_temp_std;
}
transformed data {
  int logistic = 0;

}
parameters {
  real<lower = -5, upper = 5> alpha0;

  real alpha2;   // delta_temp effect on logit(p)
  #include include/shared_parameters.stan
}
model {
  // Priors
  alpha0 ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);
  alpha2 ~ normal(0, 1); // weakly informative; delta_temp is centred

  for (t in 1:ntime) {
    // delta_temp_std[t] is a row_vector[nrec]; convert to vector for arithmetic
    vector[nrec] dt = to_vector(delta_temp_std[t]);

    for (i in 1:nind) {
      // Squared distance from activity centre to each receiver: vector[nrec]
      vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
      vector[nrec] d  = logistic ? sqrt(d2) : d2;

      // Linear predictor: intercept - distance penalty + thermal effect
      // All three terms are vector[nrec], so this is one vectorised operation.
      // binomial_logit takes the whole vector, one element per receiver.
      y[i, t] ~ binomial_logit(ntrans, alpha0 - alpha1 * d + alpha2 * dt);
    }
  }
}
generated quantities {
  real p0    = inv_logit(alpha0);
  real sigma = sqrt(1.0 / (2.0 * alpha1));
  // alpha2 is on the logit scale per unit of (centred) delta_temp.
  // To recover the effect at the original scale: effect is the same,
  // just remind yourself mean_dt was subtracted.
  real alpha2_uncentred = alpha2;   // identical numerically; here for clarity
}

to_vector(alpha0) ~ cauchy(0, 2.5);
alpha1 ~ cauchy(0, 2.5);

// Time-varying Model (alpha0 changes over time and receiver)
for (t in 1:ntime) {
  row_vector[nrec] alpha0_t = row(alpha0, t);

  for (i in 1:nind) {
    vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    vector[nrec] d = logistic ? sqrt(d2) : d2;
    
    y[i, t] ~ binomial_logit(ntrans, alpha0_t' - (alpha1 * d));
  }
}


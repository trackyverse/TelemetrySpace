alpha0 ~ cauchy(0, 2.5);
alpha1 ~ cauchy(0, 2.5);

// Standard Model (alpha0 is static)
for (t in 1:ntime) {
  for (i in 1:nind) {
    vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    vector[nrec] d = logistic ? sqrt(d2) : d2;
    
    y[i, t] ~ binomial_logit(ntrans, alpha0 - (alpha1 * d));
  }
}


data {
  #include include/shared_data.stan
}
transformed data {
  int logistic = 1;
}

// Declare parameters
parameters {
  matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; 
  #include include/shared_parameters.stan
}

// Model specification
model {
  #include include/likelihood_coa_time_varying.stan
}

generated quantities {
  // Detection probability at a distance of 0 - time-varying
  matrix[ntime, nrec] p0 = inv_logit(alpha0);
}


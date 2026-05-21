// Declare data
data {
  #include include/shared_data.stan
}

transformed data {
  int logistic = 0;
}

// Declare parameters
parameters {
  matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; // time effect
  #include include/shared_parameters.stan
}

// Model specification
model {
  #include include/likelihood_coa_time_varying.stan
} 

generated quantities {
  // Detection probability at a distance of 0 - time-varying
  matrix[ntime, nrec] p0 = inv_logit(alpha0); 
  // Standard deviation of the distance-decay function - assume constant
  real sigma = sqrt(1.0 / (2.0 * alpha1));
}


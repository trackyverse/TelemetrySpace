// Declare data
data {
  #include include/shared_data.stan
  int<lower = 0> ntest;              // number of test tags
  // number of detections from each test tag at each receiver in each time step
  array[ntest, ntime, nrec] int<lower = 0> test;
  vector[ntest] testX;               // test tag locations east-west
  vector[ntest] testY;               // test tag locations north-south
}

transformed data {
  int logistic = 0;

  // Pre-calculate squared distances from receiver for fixed test tags
  matrix[ntest, nrec] td2;
  for (s in 1:ntest) {
    td2[s, ] = to_row_vector(square(recX - testX[s]) + square(recY - testY[s]));
  }
}

// Declare parameters
parameters {
  matrix<lower = -5, upper = 5>[ntime, nrec] alpha0;
  #include include/shared_parameters.stan
}

// Model specification
model {
  // Likelihood for test tags (fixed locations)
  for (t in 1:ntime) {
    row_vector[nrec] alpha0_t = row(alpha0, t);
    
    for (s in 1:ntest) {
      // decay over distance portion of the binomial model
      vector[nrec] dist_decay = alpha1 * td2[s, ]';
      
      test[s, t] ~ binomial_logit(ntrans, row(alpha0, t)' - dist_decay);
    }
  }

  // Likelihood for individual COAs (estimated locations)
  #include include/likelihood_coa_time_varying.stan
}  

generated quantities {
   matrix[ntime, nrec] p0 = inv_logit(alpha0);
   real sigma = sqrt(1.0 / (2.0 * alpha1));
}

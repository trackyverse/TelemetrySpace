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
  int logistic = 1;
  // Pre-calculate squared distances from receiver for fixed test tags
  matrix[ntest, nrec] td;
  for (s in 1:ntest) {
    td[s, ] = to_row_vector(sqrt(square(recX - testX[s]) + square(recY - testY[s])));
  }
}

// Declare parameters
parameters {
  // detection probability intercept - max of ~1
  matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; // time effect
  #include include/shared_parameters.stan
}

// Model specification
model {
  // Likelihood for test tags (fixed locations)
  for (t in 1:ntime) {
    row_vector[nrec] alpha0_t = row(alpha0, t);
    

    for (s in 1:ntest) {
      // decay over distance portion of the binomial model
      vector[nrec] dist_decay = alpha1 * td[s, ]';
      
      test[s, t] ~ binomial_logit(ntrans, row(alpha0, t)' - dist_decay);
    }
  }

  #include include/likelihood_coa_time_varying.stan
}  

generated quantities {
   matrix[ntime, nrec] p0 = inv_logit(alpha0);
}

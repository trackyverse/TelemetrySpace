// Declare data
data {
  int<lower = 0> n_ind;               // number of individuals
  int<lower = 0> n_rec;               // number of receivers
  int<lower = 0> n_time;              // number of time steps
  int<lower = 0> n_test;              // number of test tags

  // number of trials/expected number of transmissions per time step
  int<lower = 0> n_trans;
  // number of trials/expected number of transmissions per time step for test tag
  int<lower = 0> n_trans_test;
  // number of detections for each individual at each receiver in each time step
  array[n_ind, n_time, n_rec] int<lower = 0> det;
  // number of detections from each test tag at each receiver in each time step
  array[n_test, n_time, n_rec] int<lower = 0> det_test;

  vector[n_rec] rec_x; // trap locations in east-west direction
  vector[n_rec] rec_y; // trap locations in north-south direction
  vector[2] x_lim;  // area bounds east-west
  vector[2] y_lim;                    // area boundes north-south
  vector[n_test] test_x;               // test tag locations east-west
  vector[n_test] test_y;               // test tag locations north-south
}

transformed data {
  // Pre-calculate squared distances from receiver for fixed test tags
  matrix[n_test, n_rec] td2;
  for (s in 1:n_test) {
    td2[s, ] = to_row_vector(square(rec_x - test_x[s]) + square(rec_y - test_y[s]));
  }
}

// Declare parameters
parameters {
  // fixed effects
  // detection probability intercept - max of ~1
  matrix<lower = -5, upper = 5>[n_time, n_rec] alpha0; // time effect
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  // E-W center of activity coordinate - bounds reflect spatial extent
  matrix<lower = x_lim[1], upper = x_lim[2]>[n_ind, n_time] x;
  // N-S center of activity coordinate - bounds reflect spatial extent
  matrix<lower = y_lim[1], upper = y_lim[2]>[n_ind, n_time] y;
}

// Model specification
model {
  // priors
  to_vector(alpha0) ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  // Likelihood for test tags (fixed locations)
  for (s in 1:n_test) { // For each test tag
    // decay over distance portion of the binomial model
    vector[n_rec] dist_decay = alpha1 * td2[s, ]';

    for (t in 1:n_time) { // Run binomial on logit scale
      // row(p0, t) pulls the alpha0 vector for all receivers at time t
      det_test[s, t] ~ binomial_logit(n_trans_test, row(alpha0, t)' - dist_decay);
    }
  }

  // Likelihood for individual COAs (estimated locations)
  for (i in 1:n_ind) {
    for (t in 1:n_time) {
      // Distance squared from each COA to each receiver at each time
      vector[n_rec] d2 = square(rec_x - x[i, t]) + square(rec_y - y[i, t]);
      
      // Calculate linear predictor
      vector[n_rec] lp = row(alpha0, t)' - (alpha1 * d2);
      
      // Run binomial on logit scale
      det[i, t] ~ binomial_logit(n_trans, lp);
    }
  }
}  

generated quantities {
   matrix[n_time, n_rec] p0 = inv_logit(alpha0);
   real sigma = sqrt(1.0 / (2.0 * alpha1));
}

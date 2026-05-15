// Declare data
data {
  int<lower = 0> n_ind;               // number of individuals
  int<lower = 0> n_rec;               // number of receivers
  int<lower = 0> n_time;              // number of time steps
  int<lower = 0> n_trans;             // number of trials/expected number of transmissions per time step
  array[n_ind, n_time, n_rec] int<lower = 0> det; // number of detections for each individual at each receiver in each time step
  vector[n_rec] rec_x;                 // trap locations in east-west direction
  vector[n_rec] rec_y;                 // trap locations in north-south direction
  vector[2] x_lim;                    // area bounds east-west
  vector[2] y_lim;                    // area boundes north-south
}

// Declare parameters
parameters {
  // fixed effects
  matrix<lower = -7, upper = 7>[n_time, n_rec] alpha0; // time effect
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

  // likelihood
  for (i in 1:n_ind) {
    for (t in 1:n_time) {
      // Calculate squared distance
      vector[n_rec] d2 = square(rec_x - x[i, t]) + square(rec_y - y[i, t]);

      // Run binomial on logit scale
      // row(alpha0, t) is a row_vector; ' converts to column vector
      det[i, t] ~ binomial_logit(n_trans, row(alpha0, t)' - (alpha1 * d2));
    }
  }
} 

generated quantities {
  // Detection probability at a distance of 0 - time-varying
  matrix[n_time, n_rec] p0 = inv_logit(alpha0); 
  // Standard deviation of the distance-decay function - assume constant
  real sigma = sqrt(1.0 / (2.0 * alpha1));
}


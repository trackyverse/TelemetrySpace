// Declare data
data {
  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntest;              // number of test tags

  // number of trials/expected number of transmissions per time step
  int<lower = 0> ntrans;
  // number of detections for each individual at each receiver in each time step
  array[nind, ntime, nrec] int<lower = 0> y;
  // number of detections from each test tag at each receiver in each time step
  array[ntest, ntime, nrec] int<lower = 0> test;

  vector[nrec] recX; // trap locations in east-west direction
  vector[nrec] recY; // trap locations in north-south direction
  vector[2] xlim;  // area bounds east-west
  vector[2] ylim;                    // area boundes north-south
  vector[ntest] testX;               // test tag locations east-west
  vector[ntest] testY;               // test tag locations north-south
}

transformed data {
  // Pre-calculate squared distances from receiver for fixed test tags
  matrix[ntest, nrec] td;
  for (s in 1:ntest) {
    td[s, ] = to_row_vector(sqrt(square(recX - testX[s]) + square(recY - testY[s])));
  }
}

// Declare parameters
parameters {
  // fixed effects
  // detection probability intercept - max of ~1
  matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; // time effect
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  // E-W center of activity coordinate - bounds reflect spatial extent
  matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
  // N-S center of activity coordinate - bounds reflect spatial extent
  matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
}

// Model specification
model {
  // priors
  to_vector(alpha0) ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  // Likelihood for test tags (fixed locations)
  for (s in 1:ntest) { // For each test tag
    // decay over distance portion of the binomial model
    vector[nrec] dist_decay = alpha1 * td[s, ]';

    for (t in 1:ntime) { // Run binomial on logit scale
      // row(p0, t) pulls the alpha0 vector for all receivers at time t
      test[s, t] ~ binomial_logit(ntrans, row(alpha0, t)' - dist_decay);
    }
  }

  // Likelihood for individual COAs (estimated locations)
  for (i in 1:nind) {
    for (t in 1:ntime) {
      // Distance squared from each COA to each receiver at each time
      vector[nrec] d = sqrt(square(recX - sx[i, t]) + square(recY - sy[i, t]));
      
      // Calculate linear predictor
      vector[nrec] lp = row(alpha0, t)' - (alpha1 * d);
      
      // Run binomial on logit scale
      y[i, t] ~ binomial_logit(ntrans, lp);
    }
  }
}  

generated quantities {
   matrix[ntime, nrec] p0 = inv_logit(alpha0);
}

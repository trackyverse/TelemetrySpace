// Declare data
data {
  int<lower = 0> nind;               // number of individuals
  int<lower = 0> nrec;               // number of receivers
  int<lower = 0> ntime;              // number of time steps
  int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
  array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
  vector[nrec] recX;                // receiver locations in east-west direction
  vector[nrec] recY;              // receiver locations in north-south direction
  vector[2] xlim;                    // area bounds east-west
  vector[2] ylim;                    // area boundes north-south
}

// Declare parameters
parameters {
  // fixed effects
  real<lower = -7, upper = 7> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  matrix<lower = xlim[1], upper = xlim[2]> [nind, ntime] sx; // E-W center of activity coordinate - bounds reflect spatial extent
  matrix<lower = ylim[1], upper = ylim[2]> [nind, ntime] sy; // N-S center of activity coordinate - bounds reflect spatial extent
}

model {
  // priors
  alpha0 ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  for (i in 1:nind) {
    for (t in 1:ntime) {
      // Calculate squared distance (d2)
      vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
      
      // Run binomial on logit scale
      y[i, t] ~ binomial_logit(ntrans, alpha0 - (alpha1 * d2));
    }
  }
}  

generated quantities {
  // Detection probability at a distance of 0
  // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
  real p0 = inv_logit(alpha0); 

  // Standard deviation of the distance-decay function
  // Derived from coefficient specifying distance-related decay in detection prob. 
  // (this is 1 / 2 * sigma^2 = a1) solved to equal sigma - this then is used 
  // in the full model
  real sigma = sqrt(1.0 / (2.0 * alpha1));
}

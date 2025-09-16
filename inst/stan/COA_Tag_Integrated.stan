// Declare data
data {
  int<lower = 0> n_ind;               // number of individuals
  int<lower = 0> n_rec;               // number of receivers
  int<lower = 0> n_time;              // number of time steps
  int<lower = 0> n_test;              // number of test tags
  int<lower = 0> n_trans;
  // number of trials/expected number of transmissions per time step
  array[n_ind, n_rec, n_time] int<lower = 0> det;
  // number of detections for each individual at each receiver in each time step
  array[n_test, n_rec, n_time] int<lower = 0> det_test;
  // number of detections from each test tag at each receiver in each time step
  array[n_rec] real rec_x; // trap locations in east-west direction
  array[n_rec] real rec_y; // trap locations in north-south direction
  array[2] real x_lim;  // area bounds east-west
  array[2] real y_lim;                    // area boundes north-south
  array[n_test] real test_x;               // test tag locations east-west
  array[n_test] real test_y;               // test tag locations north-south
}

// Declare parameters
parameters {
  // fixed effects
  //real<lower = -5, upper = 5> alpha0;
  // detection probability intercept - max of ~1

  array[n_time, n_rec] real<lower = -5, upper = 5> alpha0; // time effect
  real<lower = 0> alpha1;  // coef. for decline in detection probability with distance

  // latent variables
  array[n_ind, n_time] real<lower = x_lim[1], upper = x_lim[2]> x;
  // E-W center of activity coordinate - bounds reflect spatial extent
  array[n_ind, n_time] real<lower = y_lim[1], upper = y_lim[2]> y;
  // N-S center of activity coordinate - bounds reflect spatial extent
}

// Declare derived/transformed parameters
transformed parameters  {
  // Declare them
   array[n_time, n_rec] real p0;
   // Detection probability at a distance of 0 - time-varying
   real sigma;
   // Standard deviation of the distance-decay function - assume constant
   array[n_ind, n_rec, n_time] real dist;
   // Array to store distances
   array[n_test, n_rec] real dist_test;
   // Matrix of test tag distances

  // Specify them
   sigma = sqrt(1 / (2 * alpha1));
   // Derived from coefficient specifying distance-related decay in detection prob.
  // Test tag distance
  for(s in 1:n_test){ // For each test tag
    for(j in 1:n_rec){ // And each receiver
      // Calculate Euclidean distance from east test tag to each receiver
      dist_test[s, j] = sqrt(square(test_x[s] - rec_x[j]) + square(test_y[s] - rec_y[j]));
      //Calc for euclidean distance
    }
  }

  // COA distance
   for(t in 1:n_time){ // For each time step
    for(j in 1:n_rec){ // And each receiver
      for(i in 1:n_ind) { // And each individual
        // Calculate the Euclidean distance from each COA to each receiver in each time step
          dist[i, j, t] = sqrt(square(x[i, t] - rec_x[j]) + square(y[i, t] - rec_y[j]));
       // Detection probability
          //p0[t,j] = exp( alpha0 + alpha2[t,j] )/( 1+exp( alpha0 + alpha2[t,j] ) );
          p0[t, j] = inv_logit(alpha0[t, j]);
     }
   }
 }

}

// Model specification
model {
  // priors
  alpha0[n_time, n_rec] ~ cauchy(0, 2.5);
  alpha1 ~ cauchy(0, 2.5);

  // likelihood
  for (t in 1:n_time){ // For each time step
   for (j in 1:n_rec){ // And each receiver
    for (s in 1:n_test){ // And each test tag
      // Data from test tag - distance for each known
      // this needs to be adjusted for n_trans_test as it's likely that the number of 
      // transmisions from sentienal tags does not line up with the number of transmissions 
      // for normal tags that will be in a different PR/branch.
      det_test[s, j, t] ~ binomial(n_trans, p0[t, j] * exp(-alpha1 * square(dist_test[s, j])));
    }
     for (i in 1:n_ind){ // And each individual
     // Note observations (y) must be specified as an integer - otherwise will result in an error
        det[i, j, t] ~ binomial(n_trans, p0[t, j] * exp(-alpha1 * square(dist[i, j, t])));
    }
   }
  }
}  //end of model



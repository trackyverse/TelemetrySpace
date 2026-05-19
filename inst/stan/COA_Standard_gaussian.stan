// Declare data
data {
  #include include/shared_data.stan
}

transformed data {
  int logistic = 0; // Tells include file to use squared distance
}

// Declare parameters
parameters {
  // detection probability intercept on the logit scale
  // bounds are to ensure only searching reasonable parameter space
  real<lower = -5, upper = 5> alpha0;  
  #include include/shared_parameters.stan
}

model {
  #include include/likelihood_coa_static.stan
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

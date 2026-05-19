// Declare data
data {
  #include include/shared_data.stan
}
transformed data {
  int logistic = 1; // Tells include file to use linear distance (sqrt)
}
// Declare parameters
parameters {
  // fixed effects
  real<lower = -5, upper = 5> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
  #include include/shared_parameters.stan
}

model {
  #include include/likelihood_coa_static.stan
}  

generated quantities {
  // Detection probability at a distance of 0
  // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
  real p0 = inv_logit(alpha0);
}

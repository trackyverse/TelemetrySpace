# Full Stan code

In the course of developing TelemetrySpace, we realized that we were
reusing a ton of Stan code. To make this easier on ourselves, we decided
to split out the chunks we use most-often utilizing [Stan’s Includes
framework](https://mc-stan.org/docs/reference-manual/includes.html).
This is a dream for development, but makes it kind of tough to work
step-by-step through the code: before you only had to go through the
file line-by-line, whereas now you have to do this while *also* flipping
between different files.

The following vignette serves to make things easier on you (and us!) by
listing, in their completed form, the Stan code of the programs that
TelemetrySpace uses.

``` r

library(TelemetrySpace)
#> Loading required package: Rcpp
```

The Stan models used in TelemetrySpace are stored in a non-exported list
[created by
`rstantools`](https://mc-stan.org/rstantools/reference/use_rstan.html#using-the-pre-compiled-stan-programs-in-your-package),
`stanmodels`. Each element of the list contains the knitted-together
Stan code we’re looking for. We can access the code like so:

``` r

stanmodels <- TelemetrySpace:::stanmodels
## stanmodels$COA_Standard_gaussian
```

This code will be hidden for the rest of the vignette, but feel free to
poke around in the package yourself.

## COA Standard

### Gaussian transmitter signal decay

`COA_Standard(...)` or `COA_Standard(..., decay = "gaussian")`:

    #> S4 class stanmodel 'COA_Standard_gaussian' coded as follows:
    #> // Declare data
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #> }
    #> transformed data {
    #>   int logistic = 0; // Tells include file to use squared distance
    #> }
    #> // Declare parameters
    #> parameters {
    #>   // detection probability intercept on the logit scale
    #>   // bounds are to ensure only searching reasonable parameter space
    #>   real<lower = -5, upper = 5> alpha0;
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> model {
    #> alpha0 ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Standard Model (alpha0 is static)
    #> for (t in 1:ntime) {
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0 - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>   // Detection probability at a distance of 0
    #>   // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
    #>   real p0 = inv_logit(alpha0);
    #>   // Standard deviation of the distance-decay function
    #>   // Derived from coefficient specifying distance-related decay in detection prob.
    #>   // (this is 1 / 2 * sigma^2 = a1) solved to equal sigma - this then is used
    #>   // in the full model
    #>   real sigma = sqrt(1.0 / (2.0 * alpha1));
    #> }

### Logistic transmitter signal decay

`COA_Standard(..., decay = "logistic")`:

    #> S4 class stanmodel 'COA_Standard_logistic' coded as follows:
    #> // Declare data
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #> }
    #> transformed data {
    #>   int logistic = 1; // Tells include file to use linear distance (sqrt)
    #> }
    #> // Declare parameters
    #> parameters {
    #>   // fixed effects
    #>   real<lower = -5, upper = 5> alpha0;  // detection probability intercept on the logit scale - bounds are to ensure only searching reasonable parameter space
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> model {
    #> alpha0 ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Standard Model (alpha0 is static)
    #> for (t in 1:ntime) {
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0 - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>   // Detection probability at a distance of 0
    #>   // Inverse logit of alpha0 - constrains probability b/tw 0 and 1
    #>   real p0 = inv_logit(alpha0);
    #> }

## COA Time Varying

### Gaussian transmitter signal decay

`COA_TimeVarying(...)` or `COA_TimeVarying(..., decay = "gaussian")`

    #> S4 class stanmodel 'COA_TimeVarying_gaussian' coded as follows:
    #> // Declare data
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #> }
    #> transformed data {
    #>   int logistic = 0;
    #> }
    #> // Declare parameters
    #> parameters {
    #>   matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; // time effect
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> // Model specification
    #> model {
    #> to_vector(alpha0) ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Time-varying Model (alpha0 changes over time and receiver)
    #> for (t in 1:ntime) {
    #>   row_vector[nrec] alpha0_t = row(alpha0, t);
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0_t' - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>   // Detection probability at a distance of 0 - time-varying
    #>   matrix[ntime, nrec] p0 = inv_logit(alpha0);
    #>   // Standard deviation of the distance-decay function - assume constant
    #>   real sigma = sqrt(1.0 / (2.0 * alpha1));
    #> }

### Logistic transmitter signal decay

`COA_TimeVarying(..., decay = "logistic")`:

    #> S4 class stanmodel 'COA_TimeVarying_logistic' coded as follows:
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #> }
    #> transformed data {
    #>   int logistic = 1;
    #> }
    #> // Declare parameters
    #> parameters {
    #>   matrix<lower = -5, upper = 5>[ntime, nrec] alpha0;
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> // Model specification
    #> model {
    #> to_vector(alpha0) ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Time-varying Model (alpha0 changes over time and receiver)
    #> for (t in 1:ntime) {
    #>   row_vector[nrec] alpha0_t = row(alpha0, t);
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0_t' - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>   // Detection probability at a distance of 0 - time-varying
    #>   matrix[ntime, nrec] p0 = inv_logit(alpha0);
    #> }

## COA Tag-Itegrated

### Gaussian transmitter signal decay

`COA_TagInt(...)` or `COA_TagInt(..., decay = "gaussian")`:

    #> S4 class stanmodel 'COA_Tag_Integrated_gaussian' coded as follows:
    #> // Declare data
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #>   int<lower = 0> ntest;              // number of test tags
    #>   // number of detections from each test tag at each receiver in each time step
    #>   array[ntest, ntime, nrec] int<lower = 0> test;
    #>   vector[ntest] testX;               // test tag locations east-west
    #>   vector[ntest] testY;               // test tag locations north-south
    #> }
    #> transformed data {
    #>   int logistic = 0;
    #>   // Pre-calculate squared distances from receiver for fixed test tags
    #>   matrix[ntest, nrec] td2;
    #>   for (s in 1:ntest) {
    #>     td2[s, ] = to_row_vector(square(recX - testX[s]) + square(recY - testY[s]));
    #>   }
    #> }
    #> // Declare parameters
    #> parameters {
    #>   matrix<lower = -5, upper = 5>[ntime, nrec] alpha0;
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> // Model specification
    #> model {
    #>   // Likelihood for test tags (fixed locations)
    #>   for (t in 1:ntime) {
    #>     row_vector[nrec] alpha0_t = row(alpha0, t);
    #>     for (s in 1:ntest) {
    #>       // decay over distance portion of the binomial model
    #>       vector[nrec] dist_decay = alpha1 * td2[s, ]';
    #>       test[s, t] ~ binomial_logit(ntrans, row(alpha0, t)' - dist_decay);
    #>     }
    #>   }
    #>   // Likelihood for individual COAs (estimated locations)
    #> to_vector(alpha0) ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Time-varying Model (alpha0 changes over time and receiver)
    #> for (t in 1:ntime) {
    #>   row_vector[nrec] alpha0_t = row(alpha0, t);
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0_t' - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>    matrix[ntime, nrec] p0 = inv_logit(alpha0);
    #>    real sigma = sqrt(1.0 / (2.0 * alpha1));
    #> }

### Logistic transmitter signal decay

`COA_TagInt(..., decay = "logistic")`:

    #> S4 class stanmodel 'COA_Tag_Integrated_logistic' coded as follows:
    #> // Declare data
    #> data {
    #>   int<lower = 0> nind;               // number of individuals
    #>   int<lower = 0> nrec;               // number of receivers
    #>   int<lower = 0> ntime;              // number of time steps
    #>   int<lower = 0> ntrans;             // number of trials/expected number of transmissions per time step
    #>   array[nind, ntime, nrec] int<lower = 0> y; // number of detections for each individual at each receiver in each time step
    #>   vector[nrec] recX;                // receiver locations in east-west direction
    #>   vector[nrec] recY;              // receiver locations in north-south direction
    #>   vector[2] xlim;                    // area bounds east-west
    #>   vector[2] ylim;                    // area boundes north-south
    #>   int<lower = 0> ntest;              // number of test tags
    #>   // number of detections from each test tag at each receiver in each time step
    #>   array[ntest, ntime, nrec] int<lower = 0> test;
    #>   vector[ntest] testX;               // test tag locations east-west
    #>   vector[ntest] testY;               // test tag locations north-south
    #> }
    #> transformed data {
    #>   int logistic = 1;
    #>   // Pre-calculate squared distances from receiver for fixed test tags
    #>   matrix[ntest, nrec] td;
    #>   for (s in 1:ntest) {
    #>     td[s, ] = to_row_vector(sqrt(square(recX - testX[s]) + square(recY - testY[s])));
    #>   }
    #> }
    #> // Declare parameters
    #> parameters {
    #>   // detection probability intercept - max of ~1
    #>   matrix<lower = -5, upper = 5>[ntime, nrec] alpha0; // time effect
    #> // fixed effect
    #> // detection probability intercept - max of ~1. [-7,7] covers 99.9% prob. space
    #> real<lower = 0> alpha1;  // coef. for decline in detection probability with distance
    #> // latent variables
    #> // E-W center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = xlim[1], upper = xlim[2]>[nind, ntime] sx;
    #> // N-S center of activity coordinate - bounds reflect spatial extent
    #> matrix<lower = ylim[1], upper = ylim[2]>[nind, ntime] sy;
    #> }
    #> // Model specification
    #> model {
    #>   // Likelihood for test tags (fixed locations)
    #>   for (t in 1:ntime) {
    #>     row_vector[nrec] alpha0_t = row(alpha0, t);
    #>     for (s in 1:ntest) {
    #>       // decay over distance portion of the binomial model
    #>       vector[nrec] dist_decay = alpha1 * td[s, ]';
    #>       test[s, t] ~ binomial_logit(ntrans, row(alpha0, t)' - dist_decay);
    #>     }
    #>   }
    #> to_vector(alpha0) ~ cauchy(0, 2.5);
    #> alpha1 ~ cauchy(0, 2.5);
    #> // Time-varying Model (alpha0 changes over time and receiver)
    #> for (t in 1:ntime) {
    #>   row_vector[nrec] alpha0_t = row(alpha0, t);
    #>   for (i in 1:nind) {
    #>     vector[nrec] d2 = square(recX - sx[i, t]) + square(recY - sy[i, t]);
    #>     vector[nrec] d = logistic ? sqrt(d2) : d2;
    #> 
    #>     y[i, t] ~ binomial_logit(ntrans, alpha0_t' - (alpha1 * d));
    #>   }
    #> }
    #> }
    #> generated quantities {
    #>    matrix[ntime, nrec] p0 = inv_logit(alpha0);
    #> }

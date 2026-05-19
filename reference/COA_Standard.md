# Fits a Bayesian Spatial Point Process model to estimate individual centers of activity from acoustic telemetry data using Stan

Fits a Bayesian Spatial Point Process model to estimate individual
centers of activity from acoustic telemetry data using Stan

## Usage

``` r
COA_Standard(
  nind,
  nrec,
  ntime,
  ntrans,
  y,
  recX,
  recY,
  xlim,
  ylim,
  decay = "gaussian",
  ndraws = NULL,
  ...
)
```

## Arguments

- nind:

  Number of tagged individuals

- nrec:

  Number of receivers

- ntime:

  Number of time steps

- ntrans:

  Number of expected transmissions per tag per time interval

- y:

  Array of detection data, where row = individual, column = time step,
  and matrix = receiver

- recX:

  Receiver coordinates in the east-west direction (should be projected
  and scaled for computational efficiency)

- recY:

  Receiver coordinates in the north-south direction (should be projected
  and scaled for computational efficiency)

- xlim:

  East-west boundaries of spatial extent (receiver array + buffer)

- ylim:

  North-south boundaries of spatial extent (receiver array + buffer).

- decay:

  desired decay function. Currently one of "gaussian" or "logistic".
  Default is "gaussian".

- ndraws:

  to be passed to `generated_quantities`. Changes the number of draws.
  Default is 10.

- ...:

  Additional arguments passed to `sampling` from `rstan`. This can
  include setting `chains`, `iter`, `warmup`, and `control`. Please see
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  for more info.

## Value

COA_Standard returns an object of class `stanfit` returned by
[`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
See the `rstan` package documentation for details.

This function returns a list containing the following components: 1) a
summary of the detection function parameters; 2) the time required for
model fitting; 3) the estimated COAs for each individual in each time
step and 95 percent credible interval; and 4) a dataframe containing
values for each parameter and latent parameter from chain iterations.
These can be used to plot posterior distributions and the credible
interval around each estimated COA.

## See also

[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)

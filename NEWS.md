# TelemetrySpace 1.3.0
  - Code refactor, **BREAKING CHANGE**: The structure is now "per individual" (loop) > "per time" (loop) > _vectorized over_ receivers. Because of this, input data must be an array with dimensions 1: individual; 2: time; 3: receiver. Previously it was individual, receiver, time.
  - `p0` and `sigma` are generated quantities rather than transformed parameters. This should not affect anything on the user side, but the code runs a bit faster.
  - Calculated distances between receivers and COAs have been moved from the transformed parameters to the model block
  - The model now operates on squared distances rather than Euclidean distance.
  - We now use the `binomial_logit` function to avoid the `logit`/`inv_logit` round-trip.

# TelemetrySpace 1.2.0

  - Add `generated_quantities` function ([PR # 3](https://github.com/trackyverse/TelemetrySpace/pull/3))

# TelemetrySpace 1.1.0

  - New package maintainers: [Ben Hlina](https://github.com/benjaminhlina) and [Mike O'Brien](https://github.com/mhpob)
  - Update Stan tooling to 2025 
  - Update vignette to more "modern" R coding styles
  - Add tests
  - Fork to the [Trackyverse GitHub Organization](https://github.com/trackyverse/TelemetrySpace)

# TelemtrySpace 1.0.0

  - Initial release of TelemetrySpace.
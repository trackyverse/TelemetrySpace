# Package index

## Models

These are the core models of the package.

- [`COA_Standard()`](https://trackyverse.github.io/TelemetrySpace/reference/COA_Standard.md)
  : Fits a Bayesian Spatial Point Process model to estimate individual
  centers of activity from acoustic telemetry data using Stan
- [`COA_TimeVarying()`](https://trackyverse.github.io/TelemetrySpace/reference/COA_TimeVarying.md)
  : Fits a time-varying Bayesian Spatial Point Process model to estimate
  individual centers of activity from acoustic telemetry data using Stan
- [`COA_TagInt()`](https://trackyverse.github.io/TelemetrySpace/reference/COA_TagInt.md)
  : Fits a test-tag integrated Bayesian Spatial Point Process model to
  estimate individual centers of activity from acoustic telemetry data
  using Stan

## Helper functions

- [`build_aeqd()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_bbox()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_counts()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_init()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_ntrans()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_pixel_grid()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_rec_coords()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_time_bin()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  [`build_tstep()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
  : Build Functions
- [`distf()`](https://trackyverse.github.io/TelemetrySpace/reference/distf.md)
  : Calculate Euclidean distance between receivers and activity centers

## Datasets

These are the test datasets shipped with the package.

- [`Y`](https://trackyverse.github.io/TelemetrySpace/reference/Y.md) :
  Counts of detection per time steps

- [`fishdat`](https://trackyverse.github.io/TelemetrySpace/reference/fishdat.md)
  : Black sea bass detection data

- [`rlocs`](https://trackyverse.github.io/TelemetrySpace/reference/rlocs.md)
  : Receiver locations from a black sea bass array

- [`example_extent`](https://trackyverse.github.io/TelemetrySpace/reference/example_extent.md)
  : Example array extent

- [`testY`](https://trackyverse.github.io/TelemetrySpace/reference/testY.md)
  : Counts of detection per time steps for test tag

- [`testdat`](https://trackyverse.github.io/TelemetrySpace/reference/testdat.md)
  : Stationary test transmitter data

- [`testloc`](https://trackyverse.github.io/TelemetrySpace/reference/testloc.md)
  : Location of a stationary test transmitter placed in the black sea
  bass array

- [`model_param_ex`](https://trackyverse.github.io/TelemetrySpace/reference/model_param_ex.md)
  : Example model parameters

- [`ps`](https://trackyverse.github.io/TelemetrySpace/reference/ps.md) :

  Parry Sound `sf` object

- [`ps_rec_loc`](https://trackyverse.github.io/TelemetrySpace/reference/ps_rec_loc.md)
  : Parry Sound - Receiver Location

- [`ps_det_example`](https://trackyverse.github.io/TelemetrySpace/reference/ps_det_example.md)
  :

  Detection `data.frame` for a tagged Lake Trout

- [`ps_det_test_tag`](https://trackyverse.github.io/TelemetrySpace/reference/ps_det_test_tag.md)
  :

  Detection `data.frame` for a test tag

- [`ps_test_tag_loc`](https://trackyverse.github.io/TelemetrySpace/reference/ps_test_tag_loc.md)
  : Parry Sound - Test Tag Locations object

## Misc

- [`TelemetrySpace-package`](https://trackyverse.github.io/TelemetrySpace/reference/TelemetrySpace-package.md)
  [`TelemetrySpace`](https://trackyverse.github.io/TelemetrySpace/reference/TelemetrySpace-package.md)
  : The 'TelemetrySpace' package.

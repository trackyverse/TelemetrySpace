# TelemetrySpace: Fit spatial point process and geostatistical mixed effects models to electronic tagging data

> [!CAUTION]
> As of August 2025, development of `TelemetrySpace` has moved to [https://github.com/trackyverse/TelemetrySpace](https://github.com/trackyverse/TelemetrySpace). Please direct any new issues or pull requests to that repository.

The `TelemetrySpace` package implements the model described in [Winton et al. 2018](https://doi.org/10.1111/2041-210X.13080), *A spatial point process model to estimate individual centres of activity from passive acoustic telemetry data*.

[![R-CMD-check](https://github.com/meganwinton/TelemetrySpace/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/meganwinton/TelemetrySpace/actions/workflows/R-CMD-check.yaml)
[![TelemetrySpace status badge](https://ocean-tracking-network.r-universe.dev/TelemetrySpace/badges/version)](https://ocean-tracking-network.r-universe.dev/TelemetrySpace)




## Installation

We suggest installing via the Ocean Tracking Network's R-universe:

```
install.packages('TelemetrySpace', repos = c('https://ocean-tracking-network.r-universe.dev', getOption("repos"))
```

The `TelemetrySpace` package uses [Stan](http://mc-stan.org/) for model fitting. If you would like to use more-up-to-date versions of Stan or [RStan](https://mc-stan.org/rstan/) dependencies, please follow the instructions at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.

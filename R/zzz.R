.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) {
    loadModule(m, what = TRUE)
  }
  rstan::rstan_options(auto_write = TRUE)
}

# ---- global variables ------
# add variables going forward in alphabetical order
utils::globalVariables(
  c(
    ".chain",
    ".data",
    ".draw",
    ".iteration",
    "2.5%",
    "97.5%",
    "alpha0",
    "alpha1",
    "coord",
    "d_probs",
    "detection_timestamp_utc",
    "easting",
    "fish",
    "geometry",
    "ind",
    "lp__",

    "max_delay",
    "median",
    "min_delay",
    "nind",
    "northing",
    "nrec",
    "ntest",
    "ntime",
    "ntrans",
    "q2.5",
    "q97.5",
    "rec",
    "recX",
    "recY",
    "sx",
    "sy",
    "sx_median",
    "sx_q2.5",
    "sx_q97.5",
    "sy_median",
    "sy_q2.5",
    "sy_q97.5",
    "tag_serial_no",
    "testX",
    "testY",
    "time",
    "time_bin",
    "value",
    "variable",
    "x_lower",
    "x_upper",
    "y",
    "y_lower",
    "y_upper"
  )
)

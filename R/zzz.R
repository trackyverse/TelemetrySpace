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
    ".data",
    "alpha0",
    "alpha1",
    "detection_timestamp_utc",
    "easting",
    "geometry",
    "max_delay",
    "min_delay",
    "nind",
    "northing",
    "nrec",
    "ntest",
    "ntime",
    "ntrans",
    "rec",
    "recX",
    "recY",
    "sx",
    "sy",
    "tag_serial_no",
    "testX",
    "testY",
    "time",
    "time_bin"
  )
)

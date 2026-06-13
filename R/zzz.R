.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) {
    loadModule(m, what = TRUE)
  }
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
}

# ---- global variables ------
# add variables going forward in alphabetical order
utils::globalVariables(
  c(
    "alpha0",
    "alpha1",
    "easting",
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
    "time"
  )
)

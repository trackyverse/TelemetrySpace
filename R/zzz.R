.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) {
    loadModule(m, what = TRUE)
  }
}

# ---- global variables ------
# add variables going forward in alphabetical order
utils::globalVariables(
  c(
    "alpha0",
    "alpha1",
    "n_ind",
    "n_rec",
    "n_test",
    "n_time",
    "n_trans",
    "rec_x",
    "rec_y",
    "x",
    "y",
    "test_x",
    "test_y"
  )
)

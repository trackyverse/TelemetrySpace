# library(testthat)
# library(TelemetrySpace)
# ---- test each argument if it errors appropriately -----

# Base arguments for COA_Standard
coa_args <- list(
  nind = model_param_ex$nind,
  nrec = model_param_ex$nrec,
  ntime = model_param_ex$tsteps,
  ntrans = model_param_ex$ntrans,
  y = Y,
  recX = rlocs$east,
  recY = rlocs$north,
  xlim = example_extent$xlim,
  ylim = example_extent$ylim,
  chains = 2,
  warmup = 1000,
  iter   = 2000,
  control = list(adapt_delta = 0.95)
)

# Helper to run COA_Standard with overridden args
call_coa <- function(overrides) {
  do.call(COA_Standard, modifyList(coa_args, overrides))
}


params_table <- list(
  list(
    param = "nind",
    bad = list("a", NA, c(1, 2)),
    regex = "`nind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "nrec",
    bad = list("a", NA, c(1, 2)),
    regex = "`nrec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntime",
    bad = list("a", NA, c(1, 2)),
    regex = "`ntime` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntrans",
    bad = list(c(model_param_ex$ntrans, model_param_ex$ntrans), "1"),
    regex = "`ntrans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "y",
    bad = list(c(1, 2, 3), "a"),
    regex = "`y` must be a 3-dimensional numeric array."
  ),
  list(
    param = "recX",
    bad   = list("a", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad   = list("a", NA),
    regex = "`recY` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "xlim",
    bad = list("a", c(1, 2, 3)),
    regex = "`xlim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "ylim",
    bad = list("a", c(1, 2, 3)),
    regex = "`ylim` must be a numeric vector that has a length of 2."
  )
)

# ----- Check Params -----

test_that("parameter validation works", {

  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch({
        expect_error(
          call_coa(setNames(list(bad_val), pt$param)),
          regexp = pt$regex,
          label = sprintf("param=%s, bad_val=%s", pt$param, deparse(bad_val))
        )
      },
      error = function(e) {
        cat("\n Error for param:", pt$param,
            " bad_val:", deparse(bad_val), "\n")
        stop(e)
      })
    }
  }
})


# ---- run model and check of it works ----
fit <- COA_Standard(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)


# bayesplot::ppc_dens_overlay(y = as.vector(Y), yrep = fit$generated_quantities)

# rstan::traceplot(fit$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))


test_that("test COA_standard model results to make sure its consisitent", {
  mean_p0 <- fit$summary[1]
  expected_mean_p0 <- 0.2818
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)

})
test_that("check to see if fit classes", {

  expect_type(fit, "list")
  expect_s4_class(fit$model, "stanfit")
  expect_s3_class(fit$coas, "data.frame")
  expect_s3_class(fit$all_estimates, "data.frame")
  expect_type(fit$summary, "double")
  expect_true(is.matrix(fit$summary))
  expect_true(is.numeric(fit$time))

})



test_that("check to see if coa returns proper info", {

  expect_true("coas" %in% names(fit))
  expect_equal(nrow(fit$coas), model_param_ex$tsteps)
  expect_equal(colnames(fit$coas), c(
    "time", "x", "y", "x_lower",
    "x_upper", "y_lower", "y_upper"
  ))

  for (col in colnames(fit$coas)) {
    expect_type(fit$coas[[col]], "double")
    expect_true(all(is.finite(fit$coas[[col]])))
  }
}
)

test_that("check to see model converged and has a good rhat", {

  rhat <- fit$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
}
)



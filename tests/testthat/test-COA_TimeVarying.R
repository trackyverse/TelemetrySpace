# ----- Model checked from setup-test-env is object time_vary_gaussian -----

# ---- test each argument if it errors appropriately -----
# ---- Check if n_ind errors -----

# Base arguments for COA_Standard
coa_args <- list(
  n_ind = model_param_ex$n_ind,
  n_rec = model_param_ex$n_rec,
  n_time = model_param_ex$tsteps,
  n_trans = model_param_ex$n_trans,
  det = Y,
  rec_x = rlocs$east,
  rec_y = rlocs$north,
  x_lim = example_extent$x_lim,
  y_lim = example_extent$y_lim,
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)

# Helper to run COA_Standard with overridden args
call_coa_timevarying <- function(overrides) {
  do.call(COA_TimeVarying, modifyList(coa_args, overrides))
}


params_table <- list(
  list(
    param = "n_ind",
    bad = list("a", NA, c(1, 2)),
    regex = "`n_ind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_rec",
    bad = list("a", NA, c(1, 2)),
    regex = "`n_rec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_time",
    bad = list("a", NA, c(1, 2)),
    regex = "`n_time` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_trans",
    bad = list(c(model_param_ex$n_trans, model_param_ex$n_trans), "1"),
    regex = "`n_trans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "det",
    bad = list(c(1, 2, 3), "a"),
    regex = "`det` must be a 3-dimensional numeric array."
  ),
  list(
    param = "rec_x",
    bad = list("a", NA),
    regex = "`rec_x` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "rec_y",
    bad = list("a", NA),
    regex = "`rec_y` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "x_lim",
    bad = list("a", c(1, 2, 3)),
    regex = "`x_lim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "y_lim",
    bad = list("a", c(1, 2, 3)),
    regex = "`y_lim` must be a numeric vector that has a length of 2."
  )
)

# ----- Check Params -----

test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_coa_timevarying(setNames(list(bad_val), pt$param)),
            regexp = pt$regex,
            label = sprintf("param=%s, bad_val=%s", pt$param, deparse(bad_val))
          )
        },
        error = function(e) {
          cat(
            "\n Error for param:",
            pt$param,
            " bad_val:",
            deparse(bad_val),
            "\n"
          )
          stop(e)
        }
      )
    }
  }
})


# ---- run model and check of it works ----
# summary(time_vary_gaussian)

# bayesplot::ppc_dens_overlay(y = as.vector(Y), yrep = time_vary_gaussian$generated_quantities)
# rstan::traceplot(time_vary_gaussian$model, pars = c(
#   # "alpha0",
#   "alpha1",
#   "sigma", "lp__"))
# time_vary_gaussian$coas
test_that("test COA_TimeVarying model results to make sure its consistent", {
  mean_p0 <- time_vary_gaussian$summary[1]
  expected_mean_p0 <- 0.4928
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.07)
})


test_that("check time_vary_gaussian classes", {
  expect_type(time_vary_gaussian, "list")
  expect_s4_class(time_vary_gaussian$model, "stanfit")
  expect_s3_class(time_vary_gaussian$coas, "data.frame")
  expect_s3_class(time_vary_gaussian$all_estimates, "data.frame")
  expect_type(time_vary_gaussian$summary, "double")
  expect_true(is.matrix(time_vary_gaussian$summary))
  expect_true(is.matrix(time_vary_gaussian$generated_quantities$yrep))
  expect_type(time_vary_gaussian$generated_quantities, "list")
  expect_true(is.numeric(time_vary_gaussian$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(time_vary_gaussian))
  expect_equal(nrow(time_vary_gaussian$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(time_vary_gaussian$coas),
    c(
      "time",
      "x",
      "y",
      "x_lower",
      "x_upper",
      "y_lower",
      "y_upper"
    )
  )

  for (col in colnames(time_vary_gaussian$coas)) {
    expect_type(time_vary_gaussian$coas[[col]], "double")
    expect_true(all(is.finite(time_vary_gaussian$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- time_vary_gaussian$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(time_vary_gaussian$generated_quantities$yrep) %in% expected)
})


#### LOGISTIC ####
test_that("test COA_TimeVarying model results to make sure its consistent", {
  mean_p0 <- time_vary_logistic$summary[1]
  expected_mean_p0 <- 0.49815
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.07)
})


test_that("check time_vary_logistic classes", {
  expect_type(time_vary_logistic, "list")
  expect_s4_class(time_vary_logistic$model, "stanfit")
  expect_s3_class(time_vary_logistic$coas, "data.frame")
  expect_s3_class(time_vary_logistic$all_estimates, "data.frame")
  expect_type(time_vary_logistic$summary, "double")
  expect_true(is.matrix(time_vary_logistic$summary))
  expect_true(is.matrix(time_vary_logistic$generated_quantities$yrep))
  expect_type(time_vary_logistic$generated_quantities, "list")
  expect_true(is.numeric(time_vary_logistic$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(time_vary_logistic))
  expect_equal(nrow(time_vary_logistic$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(time_vary_logistic$coas),
    c(
      "time",
      "x",
      "y",
      "x_lower",
      "x_upper",
      "y_lower",
      "y_upper"
    )
  )

  for (col in colnames(time_vary_logistic$coas)) {
    expect_type(time_vary_logistic$coas[[col]], "double")
    expect_true(all(is.finite(time_vary_logistic$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- time_vary_logistic$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(time_vary_logistic$generated_quantities$yrep) %in% expected)
})

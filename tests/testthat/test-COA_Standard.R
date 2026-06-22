# ----- Model checked from setup-test-env is object model_coa_standard -----

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
  iter = 2000,
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
    bad = list("a", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad = list("a", NA),
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
  ),
  list(
    param = "ndraws",
    bad = list("a", c(1, 2, 3)),
    regex = "`ndraws` must be a numeric vector that has a length of 1."
  )
)

# ----- Check Params -----

test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_coa(setNames(list(bad_val), pt$param)),
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

# standard_gaussian$generated_quantities

# bayesplot::ppc_dens_overlay(y = as.vector(Y), yrep = standard_gaussian$generated_quantities)

# rstan::traceplot(fit$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_standard gaussian model results to make sure its consistent", {
  mean_p0 <- standard_gaussian$summary[1]
  expected_mean_p0 <- 0.429
  # expected_mean_p0 <- 0.2658
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})
test_that("check standard_gaussian classes", {
  expect_type(standard_gaussian, "list")
  expect_s4_class(standard_gaussian$model, "stanfit")
  expect_s3_class(standard_gaussian$coas, "data.frame")
  expect_s3_class(standard_gaussian$all_estimates, "data.frame")
  expect_type(standard_gaussian$summary, "double")
  expect_true(is.matrix(standard_gaussian$summary))
  expect_true(is.matrix(standard_gaussian$generated_quantities$yrep))
  expect_type(standard_gaussian$generated_quantities, "list")
  expect_true(is.numeric(standard_gaussian$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(standard_gaussian))
  expect_equal(nrow(standard_gaussian$coas), time_steps)
  expect_equal(
    colnames(standard_gaussian$coas),
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

  for (col in colnames(standard_gaussian$coas)) {
    expect_type(standard_gaussian$coas[[col]], "double")
    expect_true(all(is.finite(standard_gaussian$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- standard_gaussian$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})


# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(standard_gaussian$generated_quantities$yrep) %in% expected)
})


#### LOGISTIC ####
test_that("test COA_standard logistic model results to make sure its consistent", {
  mean_p0 <- standard_logistic$summary[1]
  expected_mean_p0 <- 0.72
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})

test_that("check standard_logistic classes", {
  expect_type(standard_logistic, "list")
  expect_s4_class(standard_logistic$model, "stanfit")
  expect_s3_class(standard_logistic$coas, "data.frame")
  expect_s3_class(standard_logistic$all_estimates, "data.frame")
  expect_type(standard_logistic$summary, "double")
  expect_true(is.matrix(standard_logistic$summary))
  expect_true(is.matrix(standard_logistic$generated_quantities$yrep))
  expect_type(standard_logistic$generated_quantities, "list")
  expect_true(is.numeric(standard_logistic$time))
})

test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(standard_logistic))
  expect_equal(nrow(standard_logistic$coas), time_steps)
  expect_equal(
    colnames(standard_logistic$coas),
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

  for (col in colnames(standard_logistic$coas)) {
    expect_type(standard_logistic$coas[[col]], "double")
    expect_true(all(is.finite(standard_logistic$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- standard_logistic$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})


# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(standard_logistic$generated_quantities$yrep) %in% expected)
})

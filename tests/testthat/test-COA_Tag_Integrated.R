# library(testthat)
# library(TelemetrySpace)

nsentinal <- 1

# ---- test each argument if it errors appropriately -----
coa_args <- list(
  nind = model_param_ex$nind,
  nrec = model_param_ex$nrec,
  ntime = model_param_ex$tsteps,
  ntrans = model_param_ex$ntrans,
  ntest = nsentinal,
  y = Y,
  test = testY,
  recX = rlocs$east,
  recY = rlocs$north,
  xlim = example_extent$xlim,
  ylim = example_extent$ylim,
  testX = array(testloc$east, dim = c(nsentinal)),
  testY = array(testloc$north, dim = c(nsentinal)),
  chains = 2,
  warmup = 1000,
  iter   = 2000,
  control = list(adapt_delta = 0.95)
)
# ----- call coa_tag
call_coa_tagint <- function(overrides) {
  do.call(COA_TagInt, modifyList(coa_args, overrides))
}
# ----- create param tables
params_table <- list(
  list(
    param = "nind",
    bad = list("bc", NA, c(1, 2)),
    regex = "`nind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "nrec",
    bad = list("bc", NA, c(1, 2)),
    regex = "`nrec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntime",
    bad = list("bc", NA, c(1, 2)),
    regex = "`ntime` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntrans",
    bad = list(c(model_param_ex$ntrans, model_param_ex$ntrans), "1"),
    regex = "`ntrans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntest",
    bad = list(c(3, 6, 3), "1"),
    regex = "`ntest` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "y",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`y` must be a 3-dimensional numeric array."
  ),
  list(
    param = "test",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`test` must be a 3-dimensional numeric array."
  ),
  list(
    param = "recX",
    bad   = list("bc", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad   = list("bc", NA),
    regex = "`recY` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "xlim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`xlim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "ylim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`ylim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "testX",
    bad   = list("bc", NA),
    regex = "`testX` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  ),
  list(
    param = "testY",
    bad   = list("bc", NA),
    regex = "`testY` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  )
)

params_table
# ----- Check Params -----

test_that("parameter validation works", {

  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch({
        expect_error(
          call_coa_tagint(setNames(list(bad_val), pt$param)),
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
fit <- COA_TagInt(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim,
  ntest = nsentinal,
  test = testY,
  testX = array(testloc$east, dim = c(nsentinal)),
  testY = array(testloc$north, dim = c(nsentinal)),# N-S boundary of spatial extent (receiver array + buffer)
  chains = 2,
  warmup = 4000,
  iter = 8000,
  control = list(adapt_delta = 0.95)
)

# rstan::traceplot(fit$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_TagInt model results to make sure its consisitent", {
  mean_p0 <- fit$summary[1]

  expected_mean_p0 <- 0.486
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




test_that("check generated quantities yrep", {

  draws <- rstan::extract(fit$model)
  # check name
  expect_true("yrep" %in% names(draws))
  # check length
  expect_true(dim(draws$yrep)[1] %in% 8000)
  # for speed make this 100 if we want to check all increase this
  n_draws <- 10
  y_obs_vec <- as.vector(Y)
  n_obs <- length(y_obs_vec)
  y_rep_mat <- matrix(NA, nrow = n_draws, ncol = n_obs)

  for (i in 1:n_draws) {
    y_rep_mat[i, ] <- as.vector(draws$yrep[i, , , ])
  }
  # make sure there's no NA and make sure obs vfallls within a range
  for (i in 1:n_draws) {
    expect_false(unique(is.na( y_rep_mat[i, ])))
    expect_true(all(y_rep_mat[i, ] >= 0 &  y_rep_mat[i, ] <= 25))
  }
}
)



test_that("check generated quantities testrep", {

  draws <- rstan::extract(fit$model)
  # check name
  expect_true("testrep" %in% names(draws))
  # check length
  expect_true(dim(draws$testrep)[1] %in% 8000)
  # for speed make this 100 if we want to check all increase this

  n_draws <- 10
  y_obs_vec <- as.vector(testY)
  n_obs <- length(y_obs_vec)
  test_rep_mat <- matrix(NA, nrow = n_draws, ncol = n_obs)

  for (i in 1:n_draws) {
    test_rep_mat[i, ] <- as.vector(draws$testrep[i, , , ])
  }
  # make sure there's no NA and make sure obs vfallls within a range
  for (i in 1:n_draws) {
    expect_false(unique(is.na(test_rep_mat[i, ])))
    expect_true(all(test_rep_mat[i, ] >= 0 &  test_rep_mat[i, ] <= 29))
  }


}
)




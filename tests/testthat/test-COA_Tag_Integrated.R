# ----- Model checked from setup-test-env is object tag_int_gaussian-----

# ---- test each argument if it errors appropriately -----
coa_args <- list(
  n_ind = model_param_ex$nind,
  n_rec = model_param_ex$nrec,
  n_time = model_param_ex$tsteps,
  n_trans = model_param_ex$ntrans,
  n_test = n_sentinel,
  det = Y,
  det_test = testY,
  rec_x = rlocs$east,
  rec_y = rlocs$north,
  x_lim = example_extent$xlim,
  y_lim = example_extent$ylim,
  test_x = array(testloc$east, dim = c(n_sentinel)),
  test_y = array(testloc$north, dim = c(n_sentinel)),
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)
# ----- call coa_tag
call_coa_tagint <- function(overrides) {
  do.call(COA_TagInt, modifyList(coa_args, overrides))
}
# ----- create param tables
params_table <- list(
  list(
    param = "n_ind",
    bad = list("bc", NA, c(1, 2)),
    regex = "`nind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_rec",
    bad = list("bc", NA, c(1, 2)),
    regex = "`n_rec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_time",
    bad = list("bc", NA, c(1, 2)),
    regex = "`n_time` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_trans",
    bad = list(c(model_param_ex$ntrans, model_param_ex$ntrans), "1"),
    regex = "`n_trans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "n_test",
    bad = list(c(3, 6, 3), "1"),
    regex = "`n_test` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "det",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`det` must be a 3-dimensional numeric array."
  ),
  list(
    param = "det_test",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`det_test` must be a 3-dimensional numeric array."
  ),
  list(
    param = "rec_x",
    bad = list("bc", NA),
    regex = "`rec_x` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "rec_y",
    bad = list("bc", NA),
    regex = "`rec_y` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "x_lim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`x_lim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "y_lim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`y_lim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "test_x",
    bad = list("bc", NA),
    regex = "`test_x` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  ),
  list(
    param = "test_y",
    bad = list("bc", NA),
    regex = "`test_y` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  )
)

# params_table
# ----- Check Params -----

test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_coa_tagint(setNames(list(bad_val), pt$param)),
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

# rstan::traceplot(tag_int_gaussian$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_TagInt model results to make sure its consistent", {
  mean_p0 <- tag_int_gaussian$summary[1]

  expected_mean_p0 <- 0.5008
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})


test_that("check tag_int_gaussian classes", {
  expect_type(tag_int_gaussian, "list")
  expect_s4_class(tag_int_gaussian$model, "stanfit")
  expect_s3_class(tag_int_gaussian$coas, "data.frame")
  expect_s3_class(tag_int_gaussian$all_estimates, "data.frame")
  expect_type(tag_int_gaussian$summary, "double")
  expect_true(is.matrix(tag_int_gaussian$summary))
  expect_type(tag_int_gaussian$generated_quantities, "list")
  expect_true(is.matrix(tag_int_gaussian$generated_quantities$yrep))
  expect_true(is.matrix(tag_int_gaussian$generated_quantities$testrep))
  expect_true(is.numeric(tag_int_gaussian$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(tag_int_gaussian))
  expect_equal(nrow(tag_int_gaussian$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(tag_int_gaussian$coas),
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

  for (col in colnames(tag_int_gaussian$coas)) {
    expect_type(tag_int_gaussian$coas[[col]], "double")
    expect_true(all(is.finite(tag_int_gaussian$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- tag_int_gaussian$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(tag_int_gaussian$generated_quantities$yrep) %in% expected)
  expect_true(
    nrow(tag_int_gaussian$generated_quantities$testrep) %in% expected
  )
})


#### LOGISTIC ####
test_that("test COA_standard logistic model results to make sure its consistent", {
  mean_p0 <- tag_int_logistic$summary[1]
  expected_mean_p0 <- 0.4899
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})

test_that("check tag_int_logistic classes", {
  expect_type(tag_int_logistic, "list")
  expect_s4_class(tag_int_logistic$model, "stanfit")
  expect_s3_class(tag_int_logistic$coas, "data.frame")
  expect_s3_class(tag_int_logistic$all_estimates, "data.frame")
  expect_type(tag_int_logistic$summary, "double")
  expect_true(is.matrix(tag_int_logistic$summary))
  expect_true(is.matrix(tag_int_logistic$generated_quantities$yrep))
  expect_type(tag_int_logistic$generated_quantities, "list")
  expect_true(is.numeric(tag_int_logistic$time))
})

test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(tag_int_logistic))
  expect_equal(nrow(tag_int_logistic$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(tag_int_logistic$coas),
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

  for (col in colnames(tag_int_logistic$coas)) {
    expect_type(tag_int_logistic$coas[[col]], "double")
    expect_true(all(is.finite(tag_int_logistic$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- tag_int_logistic$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})


# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(tag_int_logistic$generated_quantities$yrep) %in% expected)
})

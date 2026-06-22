# ----- Model checked from setup-test-env is object tag_int_gaussian-----

# ---- test each argument if it errors appropriately -----
coa_args <- list(
  nind = model_param_ex$nind,
  nrec = model_param_ex$nrec,
  ntime = model_param_ex$tsteps,
  ntrans = model_param_ex$ntrans,
  ntest = nsentinel,
  y = Y,
  test = testY,
  recX = rlocs$east,
  recY = rlocs$north,
  xlim = example_extent$xlim,
  ylim = example_extent$ylim,
  testX = array(testloc$east, dim = c(nsentinel)),
  testY = array(testloc$north, dim = c(nsentinel)),
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
    bad = list("bc", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad = list("bc", NA),
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
    bad = list("bc", NA),
    regex = "`testX` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  ),
  list(
    param = "testY",
    bad = list("bc", NA),
    regex = "`testY` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
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

  expected_mean_p0 <- 0.32
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
  expect_equal(nrow(tag_int_gaussian$coas), time_steps)
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
  expected_mean_p0 <- 0.36
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
  expect_equal(nrow(tag_int_logistic$coas), time_steps)
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

# ----- Model checked from setup-test-env is object model_coa_tag_int-----

# ---- test each argument if it errors appropriately -----
coa_args <- list(
  n_ind = model_param_ex$nind,
  n_rec = model_param_ex$nrec,
  n_time = model_param_ex$tsteps,
  n_trans = model_param_ex$ntrans,
  n_test = n_sentinal,
  det = Y,
  det_test = testY,
  rec_x = rlocs$east,
  rec_y = rlocs$north,
  x_lim = example_extent$xlim,
  y_lim = example_extent$ylim,
  test_x = array(testloc$east, dim = c(n_sentinal)),
  test_y = array(testloc$north, dim = c(n_sentinal)),
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

params_table
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

# rstan::traceplot(model_coa_tag_int$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_TagInt model results to make sure its consisitent", {
  mean_p0 <- model_coa_tag_int$summary[1]

  expected_mean_p0 <- 0.486
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})


test_that("check to see if model_coa_tag_int classes", {

  expect_type(model_coa_tag_int, "list")
  expect_s4_class(model_coa_tag_int$model, "stanfit")
  expect_s3_class(model_coa_tag_int$coas, "data.frame")
  expect_s3_class(model_coa_tag_int$all_estimates, "data.frame")
  expect_type(model_coa_tag_int$summary, "double")
  expect_true(is.matrix(model_coa_tag_int$summary))
  expect_type(model_coa_tag_int$generated_quantities, "list")
  expect_true(is.matrix(model_coa_tag_int$generated_quantities$yrep))
  expect_true(is.matrix(model_coa_tag_int$generated_quantities$testrep))
  expect_true(is.numeric(model_coa_tag_int$time))

})



test_that("check to see if coa returns proper info", {

  expect_true("coas" %in% names(model_coa_tag_int))
  expect_equal(nrow(model_coa_tag_int$coas), model_param_ex$tsteps)
  expect_equal(colnames(model_coa_tag_int$coas), c(
    "time", "x", "y", "x_lower",
    "x_upper", "y_lower", "y_upper"
  ))

  for (col in colnames(model_coa_tag_int$coas)) {
    expect_type(model_coa_tag_int$coas[[col]], "double")
    expect_true(all(is.finite(model_coa_tag_int$coas[[col]])))
  }
}
)

test_that("check to see model converged and has a good rhat", {

  rhat <- model_coa_tag_int$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
}
)

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(model_coa_tag_int$generated_quantities$yrep) %in% expected)
  expect_true(nrow(model_coa_tag_int$generated_quantities$testrep) %in% expected)
}
)

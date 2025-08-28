# library(testthat)
# library(TelemetrySpace)


all_models <- list(
  model_coa_standard,
  model_coa_time_vary,
  model_coa_tag_int
)

all_data <- list(
  standata,
  standata,
  standata_1
)
# ----- check if p0 is correct demisons -----

test_that("generated_quantities produces p0_1 with correct dimensions", {

  # Example: pick first few draws to test
  ndraws_test <- 5

  for (i in seq_along(all_models)) {
    # Call your function
    out <- generated_quantities(model = all_models[[i]]$model,
                                standata = all_data[[i]],
                                ndraws = ndraws_test)

    post <- rstan::extract(all_models[[i]]$model)


    for (k in 1:ndraws_test) {
      # If p0 has >1 dimension
      if (!is.null(dim(post$p0)) && length(dim(post$p0)) %in% 3) {
        p0_1 <- post$p0[k, , ]       # [ntime, nrec]
        expect_equal(dim(p0_1), c(all_data[[i]]$ntime, all_data[[i]]$nrec))
      } else {
        p0_1 <- post$p0[k]           # scalar
        expect_length(p0_1, 1)
      }

      a1 <- post$alpha1[k]
      expect_length(a1, 1)

      check_list <- list(
        a1,
        p0_1
      )

    }
    for (l in seq_along(check_list)) {

    expect_true(all(is.numeric(check_list[[l]])))
    expect_false(any(is.na(check_list[[l]])))
    }
  }

})
# test_that("check generated quantities yrep", {
#
#   draws <- rstan::extract(model_coa_tag_int$model)
#   # check name
#   expect_true("yrep" %in% names(draws))
#   # check length
#   expect_true(dim(draws$yrep)[1] %in% 8000)
#   # for speed make this 100 if we want to check all increase this
#   n_draws <- 10
#   y_obs_vec <- as.vector(Y)
#   n_obs <- length(y_obs_vec)
#   y_rep_mat <- matrix(NA, nrow = n_draws, ncol = n_obs)
#
#   for (i in 1:n_draws) {
#     y_rep_mat[i, ] <- as.vector(draws$yrep[i, , , ])
#   }
#   # make sure there's no NA and make sure obs vfallls within a range
#   for (i in 1:n_draws) {
#     expect_false(unique(is.na( y_rep_mat[i, ])))
#     expect_true(all(y_rep_mat[i, ] >= 0 &  y_rep_mat[i, ] <= 25))
#   }
# }
# )




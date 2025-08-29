# ----- make all models into a list -----
all_models <- list(
  model_coa_standard,
  model_coa_time_vary,
  model_coa_tag_int
)
# ---- do the same for the data -----
all_data <- list(
  standata,
  standata,
  standata_1
)
# set the number of draws to test
ndraws_test <- 5
yreps <- list()


# ----- loop over generated quantities -----
for (i in seq_along(all_models)) {
  # Call your function
  yreps[[i]] <- generated_quantities(model = all_models[[i]]$model,
                                     standata = all_data[[i]],
                                     ndraws = ndraws_test)
}

# length of  each object returned bs = basic_structure
bs_returned <- c(1, 1, 2)

# length of yrep give the 5 draws
length_yrep <- ndraws_test

# -----  checks the structure of the structure of generated_quantites -------
test_that("generated_quantities returns correct structure", {

  for(s in seq_along(yreps)) {

    bs <- yreps[[s]]

    expect_type(bs, "list")
    expect_length(bs, bs_returned[s])

    for (n in seq_along(bs)) {

      post_draws <- bs[[n]]

      expect_type(post_draws, "list")
      expect_length(post_draws, length_yrep)

      for (h in seq_along(post_draws)) {

        one_draw <- post_draws[[h]]

        expect_true(is.array(one_draw))
        expect_true(any(dim(one_draw) %in% c(1, 10, 30)))
      }
    }
  }
}
)

# ------ test if numerically they make sense are are numbers ------

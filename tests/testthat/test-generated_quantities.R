# library(testthat)
# library(TelemetrySpace)
nsentinal <- 1
# ----- standata 1 ------
standata <- list(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim
)
standata_1 <- list(
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
  testY = array(testloc$north, dim = c(nsentinal))# N-S b
)

# ----- run each model ------
# ----- standard coa ------
fit_1 <- COA_Standard(
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
# ----- time integrated -----
fit_2 <- COA_TimeVarying(
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
  warmup = 3000,
  iter = 7000,
  control = list(adapt_delta = 0.95),
  seed = 4
)

# ----- tag integraged -----
fit_3 <- COA_TagInt(
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


all_models <- list(
  fit_1, fit_2,
  fit_3
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




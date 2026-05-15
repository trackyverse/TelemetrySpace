n_sentinel <- 1
# ----- standata 1 ------
standata <- list(
  n_ind = model_param_ex$nind, # number of individuals
  n_rec = model_param_ex$nrec, # number of receivers
  n_time = model_param_ex$tsteps, # number of time steps
  n_trans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  det = Y, # array of detections
  rec_x = rlocs$east, # E-W receiver coordinates
  rec_y = rlocs$north, # N-S receiver coordinates
  x_lim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  y_lim = example_extent$ylim
)
standata_test_tag <- list(
  n_ind = model_param_ex$nind, # number of individuals
  n_rec = model_param_ex$nrec, # number of receivers
  n_time = model_param_ex$tsteps, # number of time steps
  n_trans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  n_trans_test = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  det = Y, # array of detections
  rec_x = rlocs$east, # E-W receiver coordinates
  rec_y = rlocs$north, # N-S receiver coordinates
  x_lim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  y_lim = example_extent$ylim,
  n_test = n_sentinel,
  det_test = testY,
  test_x = array(testloc$east, dim = c(n_sentinel)),
  test_y = array(testloc$north, dim = c(n_sentinel)) # N-S b
)

init_fun <- function() {
  list(
    sx = matrix(
      mean(rlocs$east),
      nrow = model_param_ex$nind,
      ncol = model_param_ex$tsteps
    ),
    sy = matrix(
      mean(rlocs$north),
      nrow = model_param_ex$nind,
      ncol = model_param_ex$tsteps
    )
  )
}

# ----- run each model ------
# ----- standard coa ------
standard_gaussian <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 1500,
      iter = 2000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)
standard_logistic <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 2000,
      iter = 2500,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)
# ----- time integrated -----
time_vary_gaussian <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 3000,
      iter = 7000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)
time_vary_logistic <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 3000,
      iter = 7000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)

# ----- tag integraged -----
tag_int_gaussian <- do.call(
  COA_TagInt,
  c(
    standata_test_tag,
    list(
      chains = 2,
      warmup = 4000,
      iter = 8000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)
tag_int_logistic <- do.call(
  COA_TagInt,
  c(
    standata_test_tag,
    list(
      chains = 2,
      warmup = 4000,
      iter = 8000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)

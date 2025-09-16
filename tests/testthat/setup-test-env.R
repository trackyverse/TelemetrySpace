n_sentinal <- 1
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
standata_1 <- list(
  n_ind = model_param_ex$nind, # number of individuals
  n_rec = model_param_ex$nrec, # number of receivers
  n_time = model_param_ex$tsteps, # number of time steps
  n_trans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  det = Y, # array of detections
  rec_x = rlocs$east, # E-W receiver coordinates
  rec_y = rlocs$north, # N-S receiver coordinates
  x_lim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  y_lim = example_extent$ylim,
  n_test = n_sentinal,
  det_test = testY,
  test_x = array(testloc$east, dim = c(n_sentinal)),
  test_y = array(testloc$north, dim = c(n_sentinal)) # N-S b
)

# ----- run each model ------
# ----- standard coa ------
model_coa_standard <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      n_draws = 11
    )
  )
)
# ----- time integrated -----
model_coa_time_vary <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 3000,
      iter = 7000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      n_draws = 11
    )
  )
)

# ----- tag integraged -----
model_coa_tag_int <- do.call(
  COA_TagInt,
  c(
    standata_1,
    list(
      chains = 2,
      warmup = 4000,
      iter = 8000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      n_draws = 11
    )
  )
)

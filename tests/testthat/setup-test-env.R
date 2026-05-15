nsentinel <- 1
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
standata_testtag <- list(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim,
  ntest = nsentinel,
  test = testY,
  testX = array(testloc$east, dim = c(nsentinel)),
  testY = array(testloc$north, dim = c(nsentinel)) # N-S b
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
      warmup = 2500,
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
    standata_testtag,
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
    standata_testtag,
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




library(testthat)
library(TelemetrySpace)

nsentinal <- 1
# ---- test each argument if it errors appropriately -----
# ---- Check if nind errors -----
test_that("test nind if it errors", {

  expect_error(
    COA_TagInt(
      nind = rbind(model_param_ex$nind, model_param_ex$nind),
      ntest = nsentinal,
      # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nind' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_TagInt(
      nind = "1",  # number of individuals
      ntest = nsentinal,
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nind' must be a numeric vector that has a length of 1."
  )
}
)

# ---- Check if nind errors -----
test_that("test ntest if it errors", {

  expect_error(
    COA_TagInt(
      nind =  model_param_ex$nind,
      ntest = c(1,3),
      # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntest' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      ntest = "1",
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntest' must be a numeric vector that has a length of 1."
  )
}
)
# ---- check nrec -----
test_that("test nrec to see if it errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = rbind(model_param_ex$nrec, model_param_ex$nrec),
      ntest = nsentinal,
      # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nrec' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,
      ntest = nsentinal,
      # number of individuals
      nrec = "1", # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nrec' must be a numeric vector that has a length of 1."
  )
}
)
# ---- check ntime -----
test_that("test ntime to see if it errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,
      ntest = nsentinal,
      # number of individuals
      nrec = model_param_ex$nrec, model_param_ex$nrec, # number of receivers
      ntime = rbind(model_param_ex$tsteps, model_param_ex$tsteps), # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      # N-S boundary of spatial extent (receiver array + buffer)
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntime' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      ntest = nsentinal,
      nrec = model_param_ex$nrec, # number of receivers
      ntime = "1", # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      test = testY,
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      # E-W boundary of spatial extent (receiver array + buffer)
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntime' must be a numeric vector that has a length of 1."
  )
}
)

# ---- check ntrans -----
test_that("test ntime to see if it errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,
      ntest = nsentinal,# number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = rbind(model_param_ex$ntrans, model_param_ex$ntrans),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntrans' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec,
      ntest = nsentinal, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = "1", # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      test = testY,
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ntrans' must be a numeric vector that has a length of 1."
  )
}
)
# ---- check y -----
test_that("test y to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = c(1, 2, 3), # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'y' must be a 3-dimensional numeric array."
  )

}
)
# ---- check recX -----
test_that("test recX to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = "6", # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'recX' must be a numeric vector."
  )

}
)
# ---- check recY-----
test_that("test recY to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = "4", # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'recY' must be a numeric vector."
  )

}

)
# ---- check xlim-----
test_that("test xlim to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = c(1), # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'xlim' must be a numeric vector that has a length of 2."
  )
  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = "1", # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'xlim' must be a numeric vector that has a length of 2."
  )

}

)

# ---- check ylim-----
test_that("test ylim to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = c(1), # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ylim' must be a numeric vector that has a length of 2."
  )
  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$ylim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = "1", # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ylim' must be a numeric vector that has a length of 2."
  )

}
)
# ---- check tetsX-----
test_that("test testX to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = testloc,
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$xlim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = paste0(
      "'testX' must be a numeric array with length equal to ",
      nsentinal,
      " \\(the number of test tags\\)\\."
    )
  )
  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = "32.23",
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    # regexp = "'testX' must be a numeric array with length equal to [0-9]+ \\(the number of test tags\\)\\."

    regexp = paste0(
      "'testX' must be a numeric array with length equal to ",
      nsentinal,
      " \\(the number of test tags\\)\\."
    )

  )
  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = 3),
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    # regexp = "'testX' must be a numeric array with length equal to [0-9]+ \\(the number of test tags\\)\\."

    regexp = paste0(
      "'testX' must be a numeric array with length equal to ",
      nsentinal,
      " \\(the number of test tags\\)\\."
    )

  )

}
)


# ---- check testY-----
test_that("test testY to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = testloc,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$xlim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
      regexp = paste0(
        "'testY' must be a numeric array with length equal to ",
        nsentinal,
        " \\(the number of test tags\\)\\."
      )

  )
  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = 3),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$xlim,
      # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
      regexp = paste0(
        "'testY' must be a numeric array with length equal to ",
        nsentinal,
        " \\(the number of test tags\\)\\."
      )

  )


  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = testY,
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = "32.23",
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = paste0(
      "'testY' must be a numeric array with length equal to ",
      nsentinal,
      " \\(the number of test tags\\)\\."
    )

  )

}
)

# ---- check test-----
test_that("test test to see if errors", {

  expect_error(
    COA_TagInt(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      ntest = nsentinal,
      test = c(1, 2, 3),
      testX = array(testloc$east, dim = c(nsentinal)),
      testY = array(testloc$north, dim = c(nsentinal)),
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = c(1), # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'test' must be a 3-dimensional numeric array."
  )
}
)


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




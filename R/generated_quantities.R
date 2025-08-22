#'
#'
#' fit <- COA_Standard(
#'   nind = model_param_ex$nind, # number of individuals
#'   nrec = model_param_ex$nrec, # number of receivers
#'   ntime = model_param_ex$tsteps, # number of time steps
#'   ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
#'   y = Y, # array of detections
#'   recX = rlocs$east, # E-W receiver coordinates
#'   recY = rlocs$north, # N-S receiver coordinates
#'   xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
#'   ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
#'   chains = 2,
#'   warmup = 1000,
#'   iter = 2000,
#'   control = list(adapt_delta = 0.95)
#' )
#' fit$model
#' stan
#'
#' #' Generate Quantities
#' #'
#' #'
#' #' @param model stan model
#' #' @return generated quantites from the model
#' # generated_quantities <- function(model, standata){
#'
#'   # first we need to expose the model strucurre
#'
#'   rstan::expose_stan_functions(fit$model)
#'
#'   # extract posteriods
#'
#'   post <- rstan::extract(fit)
#'
#'   alpha0  <- post$alpha0
#'   alpha1  <- post$alpha1
#'   sx      <- post$sx
#'   sy      <- post$sy
#'
#'   # extract info standat
#'   nind  <- standata$nind
#'   nrec  <- standata$nrec
#'   ntime <- standata$ntime
#'   ntrans <- standata$ntrans
#'   recX  <- standata$recX
#'   recY  <- standata$recY
#'
#'
#'
#'   # function to create yrep for one posterior draw
#'   post_draw <- function(draw) {
#'
#'     p0 <- plogis(alpha0[draw])
#'     a1 <- alpha1[draw]
#'   }
#'
#'
#'
#'
#'
#'
#'   array[nind, nrec, ntime] int yrep; // replicated data
#'
#'   for (t in 1:ntime) {
#'     for (j in 1:nrec) {
#'       for (i in 1:nind) {
#'         real p = p0[t, j] * exp(-alpha1 * square(d[i, j, t]));
#'         // guard against tiny floating-point errors outside [0,1]
#'         p = fmin(fmax(p, 1e-9), 1 - 1e-9);
#'         yrep[i, j, t] = binomial_rng(ntrans, p);
#'       }
#'     }
#'   }
#' }
#'
#' array[nind, nrec, ntime] int yrep; // replicated data
#'
#' for (t in 1:ntime) {
#'   for (j in 1:nrec) {
#'     for (i in 1:nind) {
#'       real p = p0 * exp(-alpha1 * square(d[i, j, t]));
#'       // guard against tiny floating-point errors outside [0,1]
#'       p = fmin(fmax(p, 1e-9), 1 - 1e-9);
#'       yrep[i, j, t] = binomial_rng(ntrans, p);
#'     }
#'   }
#' }
#' }
#'
#'
#' array[ntest, nrec, ntime] int testrep; // replicated test data
#'
#'
#'
#' // replicate test tags
#' for (t in 1:ntime) {
#'   for (j in 1:nrec) {
#'     for (s in 1:ntest) {
#'       real ptest = p0[t, j] * exp(-alpha1 * square(td[s, j]));
#'       ptest = fmin(fmax(ptest, 1e-9), 1 - 1e-9);
#'       testrep[s, j, t] = binomial_rng(ntrans, ptest);
#'     }
#'   }
#' }
#' }

# }

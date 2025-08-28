#' Generate Quantities
#'
#' Used internally to calculate generated quantities for each draw
#'
#' @param model Stan model object
#' @param standata Data fed to Stan model
#' @param ndraws number of draws
#'
#' @return generated quantities from the model
#' @keywords internal
#' @name generated_quantities

generated_quantities <- function(model,
                                 standata,
                                 ndraws = NULL) {
  check_stan_object(model)
  check_numeric(ndraws)

  check_test_tag <- "ntest" %in% names(standata)
  # Set default number of draws
  if (is.null(ndraws)) {
    ndraws <- 10
  }

  # extract posteriors
  post <- rstan::extract(model)

  # pull everything out from post and put into function environment
  list2env(post, envir = environment())

  # pull everything from standata
  list2env(standata, envir = environment())

  # create vector to loop over
  ndraw <- seq_len(ndraws)
  # list to dump stuf into
  out_list <- vector("list", length = ndraws)

  for (k in seq_along(ndraw)) {
    # grab extracted values for ndarws
    draw <- ndraw[k]
    p0_1 <- p0[k]
    a1 <- alpha1[k]
    # create blank array with the name of eveyrhting
    out <- array(NA, c(nind, nrec, ntime),
                 dimnames = list(
                   tag  = seq_len(nind),
                   rec  = seq_len(nrec),
                   time = seq_len(ntime)
                 )
    )

    # if yrep is choosen us d else use td
    # if (mode %in% "yrep") {
    #   d1 <- d[draw]
    # } else {
    #   td1 <- td[draw]
    # }

    # generate quantities
    for (t in 1:ntime) {
      for (i in 1:nind) {
        for (j in 1:nrec) {
          # create distances
          d <- sqrt((sx[draw, i, t] - recX[j]) ^ 2 +
                      (sy[draw, i, t] - recY[j]) ^ 2)
          # make this work for when p0 is dimensions
          base <- if (length(dim(p0_1)) %in% 2) p0[t, j] else p0_1
          # create pobablity
          p <- base * exp(-a1 * d ^ 2)
          # make sure the pobablity is above 0
          p <- min(max(p, 1e-9), 1 - 1e-9)
          # then run int using a the iteration of transmission by probality
          # to get the number of detections
          out[i, j, t] <- rbinom(1, ntrans, p)
        }
      }
    }
    out_list[[k]] <- out
  }
  return(out_list)
}

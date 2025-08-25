#' Calculate posterior draws
#' @param ndraws The number of draws desired. `numerical`.
#' @param mode a character either `yrep` or `testrep` that controls which
#' posterior draw is pulled.



posterior_draw <- function(draws,
                           mode = NULL) {
  # make yrep null
  if (is.null(mode)) {
    mode <- "yrep"
  }
  # create vector to loop over
  ndraws <- 1:draws
  # list to dump stuf into
  out_list <- vector("list", length = draws)


  for (k in seq_along(ndraws)) {
    # grab extracted values for ndarws
    draw <- ndraws[k]
    p0_1 <- p0[draw]
    a1 <- alpha1[draw]
    # create blank array
    out <- array(NA, c(nind, nrec, ntime))
    # if yrep is choosen us d else use td
    if (mode %in% "yrep") {
      d1 <- d[draw]
    } else {
      td1 <- td[draw]
    }

    # generate qunatities
    for (t in 1:ntime) {
      for (i in 1:nind) {
        for (j in 1:nrec) {
          base <- if (length(dim(p0_1)) %in% 2) p0[t, j] else p0_1

          p <- base * exp(-a1 * d1 ^ 2)
          # p <- base * exp(-a1 * td1[j, i] ^ 2)
          p <- min(max(p, 1e-9), 1 - 1e-9)
          out[i, j, t] <- rbinom(1, ntrans, p)
        }
      }
    }
    out_list[[k]] <- out
  }


  return(out_list)
}


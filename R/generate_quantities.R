#' Generate Quantities
#'
#' @param model Stan model object
#' @param standata Data fed to Stan model
#' @param ndraws number of darws
#' @param mode `yrep` or `testrep` for predictive draws.
#'
#' @return generated quantities from the model
generate_quantities <- function(
    model,
    standata,
    ndraws = NULL
) {
  # Set default number of draws
  if (is.null(ndraws)){
    ndraws <- 10
  }

  # extract posteriors
  post <- rstan::extract(fit$model)

  # pull everything out from post and put into function enviornment
  list2env(post, envir = environment())

  # pull everything from standata
  list2env(fit$data, envir = environment())

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


yreps
obs_vec <- as.vector(obs)
n_obs <- length(obs_vec)
# rep_mat <- matrix(NA, nrow = 10, ncol = n_obs)
rep_mat <- matrix(NA, nrow = 200, ncol = n_obs)

rep_mat <- do.call(rbind, lapply(yreps, function(x) as.vector(x)))
rownames(rep_mat) <- paste0("yrep_", seq_along(yreps))

# col names = flattened index (tag, rec, time)
dim_x <- dim(yreps[[1]])
grid <- expand.grid(
  tag  = seq_len(dim_x[1]),
  rec  = seq_len(dim_x[2]),
  time = seq_len(dim_x[3])
)

colnames(rep_mat) <- apply(grid, 1, function(idx) {
  paste0("tag_", idx[1], "_rec_", idx[2], "_time_", idx[3])
})

# rep_mat[1:2, 1:10]  # first 10 observations for draws 1 and 2


# for (i in 1:dim(yreps)[1]) {
#   rep_mat[i, ] <- as.vector(yreps[i, , , ])
# }
# for (i in 1:10) {
#   rep_mat[i, ] <- as.vector(yreps[[i]])
# }

dim(rep_mat)   # should be 10 x 300
length(obs_vec)



pp_checks <- bayesplot::ppc_dens_overlay(y = obs_vec, yrep = rep_mat)

pp_checks


#
#
#
#     array[nind, nrec, ntime] int yrep; // replicated data
#
#   for (t in 1:ntime) {
#     for (j in 1:nrec) {
#       for (i in 1:nind) {
#         real p = p0[t, j] * exp(-alpha1 * square(d[i, j, t]));
#         // guard against tiny floating-point errors outside [0,1]
#         p = fmin(fmax(p, 1e-9), 1 - 1e-9);
#         yrep[i, j, t] = binomial_rng(ntrans, p);
#       }
#     }
#   }
# }
#
# array[nind, nrec, ntime] int yrep; // replicated data
#
# for (t in 1:ntime) {
#   for (j in 1:nrec) {
#     for (i in 1:nind) {
#       real p = p0 * exp(-alpha1 * square(d[i, j, t]));
#       // guard against tiny floating-point errors outside [0,1]
#       p = fmin(fmax(p, 1e-9), 1 - 1e-9);
#       yrep[i, j, t] = binomial_rng(ntrans, p);
#     }
#   }
# }
# }
#
#
# array[ntest, nrec, ntime] int testrep; // replicated test data
#
#
#
# // replicate test tags
# for (t in 1:ntime) {
#   for (j in 1:nrec) {
#     for (s in 1:ntest) {
#       real ptest = p0[t, j] * exp(-alpha1 * square(td[s, j]));
#       ptest = fmin(fmax(ptest, 1e-9), 1 - 1e-9);
#       testrep[s, j, t] = binomial_rng(ntrans, ptest);
#     }
#   }
# }
# }
#
# }

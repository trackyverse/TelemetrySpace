#' Generate Quantities
#'
#' Used internally to calculate generated quantities for each draw
#'
#' @param model Stan model object
#' @param standata Data fed to Stan model
#' @param n_draws  is the number of draws to take. Default to 10.
#'
#' @return generated quantities from the model
#' @keywords internal
#' @name generated_quantities

generated_quantities <- function(model, standata, n_draws = NULL) {
  # check stan object
  check_stan_object(model)
  # check to see if n_test is in standata this will allow for gq's to be made
  # for test tag
  check_test_tag <- "n_test" %in% names(standata)
  # Set default number of draws
  if (is.null(n_draws)) {
    n_draws <- 10
  }

  check_num_vec_len(n_draws, vec_length = 1)
  # extract posteriors
  post <- rstan::extract(model)

  # pull everything out from post and put into function environment
  list2env(post, envir = environment())

  # pull everything from standata
  list2env(standata, envir = environment())

  # create vector to loop over
  n_draw <- seq_len(n_draws)
  # list to dump stuf into
  y_rep_list <- vector("list", length = n_draws)

  if (check_test_tag) {
    y_rep_test_list <- vector("list", length = n_draws)
  }

  for (k in seq_along(n_draw)) {
    # grab extracted values for ndarws
    draw <- n_draw[k]
    a1 <- alpha1[k]

    if (length(dim(alpha0)) %in% 3) {
      # p0 has shape [n_draws, n_time, n_rec]
      p0 <- stats::plogis(alpha0[draw, , ])
    } else {
      p0 <- stats::plogis(alpha0[draw])
    }
    # create blank array with the name of eveyrhting

    y_rep <- array(
      NA,
      c(n_ind, n_rec, n_time),
      dimnames = list(
        tag = seq_len(n_ind),
        rec = seq_len(n_rec),
        time = seq_len(n_time)
      )
    )

    if (check_test_tag) {
      y_rep_test <- array(
        NA,
        c(n_test, n_rec, n_time),
        dimnames = list(
          tag = seq_len(n_test),
          rec = seq_len(n_rec),
          time = seq_len(n_time)
        )
      )
    }
    # ----- generate quantities ------
    # First for number of detections for each tagged individual
    for (t in 1:n_time) {
      for (i in 1:n_ind) {
        for (j in 1:n_rec) {
          # create distances
          dist <- sqrt(
            (x[draw, i, t] - rec_x[j])^2 +
              (y[draw, i, t] - rec_y[j])^2
          )
          # make this work for when p0 is dimensions
          if (is.matrix(p0)) {
            base <- p0[t, j]
          } else {
            base <- p0
          }
          p <- base * exp(-a1 * dist^2)
          # make sure the pobablity is above 0
          p <- min(max(p, 1e-9), 1 - 1e-9)
          # then run int using a the iteration of transmission by probability
          # to get the number of detections
          y_rep[i, j, t] <- stats::rbinom(1, n_trans, p)
        }
      }
    }
    y_rep_list[[k]] <- y_rep

    # ----- run generated quantities for n_test ------
    if (check_test_tag) {
      for (l in 1:n_time) {
        for (m in 1:n_rec) {
          for (s in 1:n_test) {
            # Euclidean distance between test tag s and receiver m
            test_dist <- sqrt(
              (test_x[s] - rec_x[m])^2 + (test_y[s] - rec_y[m])^2
            )
            # Probability
            p_test <- p0[l, m] * exp(-a1 * test_dist^2)
            p_test <- min(max(p_test, 1e-9), 1 - 1e-9)
            # Simulate detection
            y_rep_test[s, m, l] <- stats::rbinom(1, n_trans_test, p_test)
          }
        }
      }
      y_rep_test_list[[k]] <- y_rep_test
    }
  }
  if (check_test_tag) {
    return(list(y_rep = y_rep_list, test_rep = y_rep_test_list))
  } else {
    return(list(y_rep = y_rep_list))
  }
}

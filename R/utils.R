#' Error functions
#'
#' @param x is a vector to pass to check
#' @param vec_length is the length of the vector to check default is 1.
#' @param arg_name the name of the argument to check
#'
#'
#' @keywords internal
#' @name error_functions


check_num_vec_len <- function(x, vec_length = NULL, arg_name = NULL) {

  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.numeric(x) || !is.vector(x) || length(x) != vec_length) {
    cli::cli_abort("`{arg_name}` must be a numeric vector that has a length of {vec_length}.")
  }
}

#' @param x is a vector to pass to check
#' @param arg_name the name of the argument to check
#'
#' @keywords internal
#' @name error_functions

check_array <- function(x, arg_name = NULL) {

  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.array(x) || length(dim(x)) != 3 || !is.numeric(x)) {
    cli::cli_abort("`{arg_name}` must be a 3-dimensional numeric array.")
  }

}


#' @param x is a vector to pass to check
#' @param len is the length of the to make the array. This needs to be the
#' same length as `ntest` or the number of tags.
#' @param arg_name the name of the argument to check
#'
#' @keywords internal
#' @name error_functions
#'
check_array_tag <- function(x, len, arg_name = NULL) {

  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.numeric(x) || is.null(dim(x)) ||  length(x) != len) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric array with length equal to {.val {len}} (the number of test tags)."
    )
  }
}


#' expected lengths of variables in standata
#'
#' @param recX is the receiver or station y coordinates
#' @param recY is the receiver or station y coordinates
#' @param ntest is the number of reference tags
#'
#'
#' @keywords internal
#' @name expected_lengths

expected_lengths <- function(recX = NULL,
                             recY = NULL,
                             ntest = NULL) {
  lengths <- list(
    nind = 1,
    nrec = 1,
    ntime = 1,
    ntrans = 1,
    ntest = 1,
    recX = length(recX),
    recY = length(recY),
    xlim = 2,
    ylim = 2,
    testX = ntest,
    testY = ntest
  )
  return(lengths)
}

#' Validate standata
#'
#' @param standata is a list of data that will be supplied to the model
#' @param lengths is the length of each object
#'
#'
#' @keywords internal
#' @name vaidate_standata

validate_standata <- function(standata, lengths) {

  array_vars <- intersect(c("y", "test", "testX",
                            "testY"), names(standata))

  for (var in array_vars) {
    # check station locations
    if (var %in% c("testX", "testY")) {
      check_array_tag(standata[[var]], len = lengths[[var]], arg_name = var)
    } else {
      # Check 3d array used for counts
      check_array(standata[[var]], arg_name = var)
    }
  }

  # check vectors
  mapply(FUN = function(len, name) {

    if (!(name %in% array_vars) && !is.null(len) && !is.null(standata[[name]])) {

      check_num_vec_len(standata[[name]],
                        vec_length = len,
                        arg_name = name)
    }
  },
  lengths, names(lengths)
  )
}



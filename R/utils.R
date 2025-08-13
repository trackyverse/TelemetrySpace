#' Error functions
#'
#' @param x is a vector to pass to check
#' @param vec_length is the length of the vector to check default is 1.
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

#' Error functions
#'
#' @param x is a vector to pass to check
#' @param vec_length is the length of the vector to check default is 1.
#'
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


#' expected lengths of variables in standata
#'
#' @param recX is the receiver or station y coordinates
#' @param recY is the receiver or station y coordinates
#'
#'
#' @keywords internal
#' @name expected_lengths

expected_lengths <- function(recX = NULL,
                             recY = NULL) {
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
    testX = 1,
    testY = 1
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

  array_vars <- intersect(c("y", "test"), names(standata))

  # Check arrays
  purrr::walk(array_vars, ~{
    check_array(standata[[.x]], arg_name = .x)  # Your existing check
  })

  purrr::imap(lengths, function(len, name) {

    if (!(name %in% array_vars) && !is.null(len) && !is.null(standata[[name]])) {

      check_num_vec_len(standata[[name]],
                        vec_length = len,
                        arg_name = name)
    }
  })
}



#' Error functions
#'
#' @param x is a `vector`` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
#'

check_aeqd <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  check_sf_object(x)

  x_crs <- sf::st_crs(x)

  if (!grepl("\\+proj=aeqd", x_crs$input)) {
    cli::cli_abort(
      "x" = "`{arg_name}` must be in Azimuthal Equal Distance projection",
      "i" = "Use {.code build_aeqd()} then {.code sf::st_transform()} to reproject into 
       Azimuthal Equal Distance projection"
    )
  }
}

#' @param x is a `vector`` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_array <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.array(x) || !is.numeric(x) || length(dim(x)) != 3) {
    cli::cli_abort("`{arg_name}` must be a 3-dimensional numeric array.")
  }
}


#' @param x is a `vector`` to pass to check.
#' @param len is the length to make the array. This needs to be the
#' same length as `ntest` or the number of tags.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_array_tag <- function(x, len, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.array(x) || !is.numeric(x) || length(x) != len) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric array with length equal to {.val {len}} (the number of test tags)."
    )
  }
}

check_aeqd <- function(x, arg_name = NULL) {}
#' @param x is a `vector`` to pass to check.
#' @param vec_length is the length of the `vector`` to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_char_vec_len <- function(x, vec_length = NULL, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.character(x) || !is.vector(x) || length(x) != vec_length) {
    cli::cli_abort(
      "`{arg_name}` must be a charcter vector that has a length of {vec_length}."
    )
  }
}

#' @param x is a `data.frame` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_column_names <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }
  # right now this is the accepted names but we will changes this likely to ATO names
  # required_any <- list(
  #   timestamp = c("time", "detection_timestamp_utc"),
  #   receiver = c("rec", "station_no"),
  #   tag = c("tag_serial_no")
  # )

  # time_bin

  required_any <- list(
    timestamp = c("time", "detection_timestamp_utc", "time_bin"),
    receiver = c("rec", "station_no"),
    tag = c("tag_serial_no", "min_delay", "max_delay")
  )
  # accepted_names <- c(
  #   "tag_serial_no",
  #   "rec",
  #   "time"
  # )

  missing_groups <- names(Filter(
    \(aliases) !any(aliases %in% names(x)),
    required_any
  ))

  if (length(missing_groups) > 0) {
    missing_detail <- vapply(
      missing_groups,
      \(g) {
        aliases <- required_any[[g]]
        cli::format_inline(
          "{.field {g}}: needs to be named 
        one of the folowing: {.or {.val {aliases}}}"
        )
      },
      character(1)
    )

    cli::cli_abort(c(
      "`{arg_name}` is missing required colummn",
      "i" = setNames(missing_detail, rep("x", length(missing_detail)))
    ))
  }
}

#' @param x is a `data.frame` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_column_type <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }
  # right now this is the accepted names but we will changes this likely to ATO names

  accepted_numeric <- c("rec", "time", "min_delay", "max_delay")

  accepted_character <- c("tag_serial_no", "station_no")

  datetime_cols <- c("detection_timestamp_utc")

  bad_numeric <- check_present(x, accepted_numeric, is.numeric, "numeric")

  bad_character <- check_present(
    x,
    accepted_character,
    is.character,
    "character"
  )

  bad_datetime <- check_present(
    x,
    datetime_cols,
    \(col) inherits(col, "POSIXct"),
    "POSIXct"
  )
  if (length(bad_numeric) > 0) {
    cli::cli_abort(c(
      "`{arg_name}` contains columns with incorrect types.",
      "x" = "Expected numeric: {.field {bad_numeric}}"
    ))
  }
  if (length(bad_character) > 0) {
    cli::cli_abort(c(
      "`{arg_name}` contains columns with incorrect types.",
      "x" = "Expected character: {.field {bad_character}}"
    ))
  }
  if (length(bad_datetime) > 0) {
    cli::cli_abort(c(
      "`{arg_name}` contains columns with incorrect types.",
      "x" = "Expected POSIXct: {.field {bad_datetime}}"
    ))
  }
}

#' @param x is a `data.frame` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_data_frame <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!(inherits(x, c("data.frame", "tibble", "data.table")))) {
    cli::cli_abort(c(
      "`{arg_name}` must be a data.frame, tibble, or data.table",
      "i" = "Please provide data.frame"
    ))
  }
}
#' @param x is a `list` to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_list <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!(inherits(x, c("list")))) {
    cli::cli_abort(c(
      "`{arg_name}` must be a list",
      "i" = "Please provide a list"
    ))
  }
}


#' @param x object to check.
#' @param arg_name the name of the argument to check.
#'
#' @name error_functions
check_numerical <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.numeric(x) || length(x) != 1) {
    cli::cli_abort(c(
      "`{arg_name}` argument must be a numerical value.",
      "i" = "Please provide a numerical value"
    ))
  }
}
#' @param x is a `vector`` to pass to check.
#' @param vec_length is the length of the `vector`` to check.
#' @param arg_name the name of the argument to check.
#'
#'
#' @keywords internal
#' @name error_functions

check_num_vec_len <- function(x, vec_length = NULL, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.numeric(x) || !is.vector(x) || length(x) != vec_length) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric vector that has a length of {vec_length}."
    )
  }
}

#' @param x is a `data.frame` to pass to check.
#' @param cols `vector` containing the the columns the check
#' @param fnct name of function to use for example `is.numeric`
#' @param label `character` labeling the check
#'
#' @keywords internal
#' @name error_functions
check_present <- function(x, cols, fnct, label) {
  fnct <- match.fun(fnct)
  present <- intersect(cols, names(x))
  present[!vapply(x[present], fnct, logical(1))]
}


#' @param x is a `sf` object
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_sf_object <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  # valid classes from rstan and cmdstanr
  valid_classes <- c(
    "sf"
  )

  if (!inherits(x, valid_classes)) {
    cli::cli_abort(
      "`{arg_name}` must be a sf object (from {.pkg sf})."
    )
  }
}


#' @param x is a `Stan` object
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_stan_object <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  # valid classes from rstan and cmdstanr
  valid_classes <- c(
    "stanfit",
    "stanmodel",
    "CmdStanMCMC",
    "CmdStanMLE",
    "CmdStanVB",
    "CmdStanModel"
  )

  if (!inherits(x, valid_classes)) {
    cli::cli_abort(
      "`{arg_name}` must be a Stan object (from {.pkg rstan} or {.pkg cmdstanr})."
    )
  }
}

#' @param x is a `Stan` object
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_unit <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.character(x) || length(x) != 1) {
    cli::cli_abort(c(
      "`{arg_name}` must be a single character string.",
      "i" = "e.g. \"1 hour\", \"15 minutes\", \"1 day\""
    ))
  }

  tryCatch(
    lubridate::floor_date(Sys.time(), unit = x),
    error = function(e) {
      cli::cli_abort(c(
        "`{arg_name}` is not a valid {.fn lubridate::floor_date} unit: {.val {x}}",
        "i" = "e.g. \"1 hour\", \"15 minutes\", \"1 day\""
      ))
    }
  )

  invisible(x)
}

#' @param x is a `Stan` object
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_utm <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }
  crs <- sf::st_crs(x)

  if (is.na(crs)) {
    cli::cli_abort(
      "{.arg {arg_name}} has no CRS set. Please set CRS using {.fnct {sf::st_crs()}}.",
      call = NULL
    )
  }

  # UTM zones are EPSG:32601-32660 (N) and EPSG:32701-32760 (S)
  crs_extract <- as.integer(gsub("^EPSG:", "", crs$input))

  is_utm <- !is.na(crs_extract) &&
    ((crs_extract >= 32601L && crs_extract <= 32660L) ||
      (crs_extract >= 32701L && crs_extract <= 32760L))

  if (!is_utm) {
    cli::cli_alert_warning(
      c(
        "{.arg {arg_name}} is currently not in UTMs (EPSG:32601-32660 or EPSG:32701-32760), potentially 
        making distance calculations inaccurate. Are you sure this is correct? ",
        "i" = "Current CRS: {.val {crs$input}} ",
        "i" = "To transform call {.code sf::st_transform({arg_name}, <utm_epsg>)}."
      )
    )
  }

  invisible(x)
}

#' Expected lengths of variables in `standata`
#'
#' @param recX is the receiver or station x coordinates (e.g, lon).
#' @param recY is the receiver or station y coordinates (e.g., lat).
#' @param ntest_len is the number of reference tags which is used as length
#' by `testX` and `testY`.
#'
#'
#' @keywords internal
#' @name expected_lengths

expected_lengths <- function(recX = NULL, recY = NULL, ntest_len = NULL) {
  if (!is.null(ntest_len)) {
    check_num_vec_len(ntest_len, vec_length = 1, arg_name = "ntest")
  }

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
    testX = ntest_len,
    testY = ntest_len
  )
  return(lengths)
}

#' Validate `standata`
#'
#' @param standata is a list of data that will be supplied to the model.
#' @param lengths is the length of each object.
#'
#'
#' @keywords internal
#' @name vaidate_standata

validate_standata <- function(standata, lengths) {
  array_vars <- intersect(c("y", "test", "testX", "testY"), names(standata))

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
  mapply(
    FUN = function(len, name) {
      if (
        !(name %in% array_vars) && !is.null(len) && !is.null(standata[[name]])
      ) {
        check_num_vec_len(standata[[name]], vec_length = len, arg_name = name)
      }
    },
    lengths,
    names(lengths)
  )
}

#' Transform classes and structure of the output of different data objects
#'
#' Transforms output of `generated_quantities()`,
#' @param input list of three dimensional array
#'
#' @return a `matrices` of generated quantities.
#'
#' @keywords internal
#' @name transform_objects

transform_gq <- function(input) {
  # first grab the names of the input
  post_type <- names(input)

  # loop over each object in input and grab the names as
  # well as the actual input.
  output <- lapply(seq_along(input), function(i) {
    group_name <- post_type[i]
    open_input <- input[[i]]

    # check arrays to ensure that they are 3 demisions
    lapply(open_input, check_array)

    # move into matrix
    rep_mat <- do.call(rbind, lapply(open_input, as.vector))

    # rownames with group name + index
    rownames(rep_mat) <- paste0(group_name, "_", seq_along(open_input))

    # start grabbing col names
    dim_x <- dim(open_input[[1]])

    grid <- expand.grid(
      tag = seq_len(dim_x[1]),
      rec = seq_len(dim_x[2]),
      time = seq_len(dim_x[3])
    )

    # add in col names
    colnames(rep_mat) <- apply(grid, 1, function(idx) {
      paste0("tag_", idx[1], "_rec_", idx[2], "_time_", idx[3])
    })
    return(rep_mat)
  })
  names(output) <- post_type
  output
}

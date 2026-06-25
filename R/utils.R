#' Error functions
#'
#' @param sf is a `sf` object that needs to be checked.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @rdname error_functions

check_aeqd <- function(sf, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(sf))
  }

  wkt <- sf::st_crs(sf)$wkt

  if (
    is.null(wkt) ||
      is.na(wkt) ||
      !grepl("Azimuthal.Equidistant", wkt, ignore.case = TRUE)
  ) {
    cli::cli_abort(
      c(
        "x" = "`{arg_name}` must be in Azimuthal Equal Distance projection",
        "i" = "Use {.code build_aeqd()} then {.code sf::st_transform()} to reproject
      into Azimuthal Equal Distance projection."
      )
    )
  }
}

#' @param vec is a `vector` that needs to be checked.
#' @keywords internal
#' @rdname error_functions

check_aeqd_string <- function(vec, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (!(grepl("+proj=aeqd", vec))) {
    cli::cli_abort(
      c(
        "x" = "`{arg_name}` must be in Azimuthal Equal Distance projection",
        "i" = "Use {.code build_aeqd()} then supply {`arg_name`} with the 
        Azimuthal Equal Distance projection string"
      )
    )
  }
}

#' @param array is a `array` that needs to be checked.
#' @keywords internal
#' @rdname error_functions

check_array <- function(array, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(array))
  }

  if (!is.array(array) || !is.numeric(array) || length(dim(array)) != 3) {
    cli::cli_abort("`{arg_name}` must be a 3-dimensional numeric array.")
  }
}


#' @param len is the length to make the array. This needs to be the
#' same length as `ntest` or the number of tags.
#' @keywords internal
#' @rdname error_functions
#'
check_array_tag <- function(array, len, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(array))
  }

  if (!is.array(array) || !is.numeric(array) || length(array) != len) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric array with length equal to
      {.val {len}} (the number of test tags)."
    )
  }
}

#' @param vec_length is the length of the vector to check.
#' @keywords internal
#' @rdname error_functions
#'
check_char_vec_len <- function(vec, vec_length = NULL, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (!is.character(vec) || !is.vector(vec) || length(vec) != vec_length) {
    cli::cli_abort(
      "`{arg_name}` must be a charcter vector that has a length of {vec_length}."
    )
  }
}

#' @param df is a `data.frame` object that needs to be checked.
#' @keywords internal
#' @rdname error_functions

check_column_names <- function(df, arg_name = NULL, coords = FALSE) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }

  if (isFALSE(coords)) {
    required_any <- list(
      timestamp = c("time", "detection_timestamp_utc", "time_bin"),
      receiver = c("rec", "station_no"),
      tag = c("tag_serial_no", "min_delay", "max_delay")
    )
  }
  if (isTRUE(coords)) {
    required_any <- list(
      coords_x = c("recX"),
      coords_y = c("recY")
    )
  }

  missing_groups <- names(Filter(
    \(aliases) !any(aliases %in% names(df)),
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
      "i" = stats::setNames(missing_detail, rep("x", length(missing_detail)))
    ))
  }
}

#' @keywords internal
#' @rdname error_functions

check_column_type <- function(df, arg_name = NULL, coords = FALSE) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }
  # right now this is the accepted names but we will changes this likely to ATO names

  if (isFALSE(coords)) {
    accepted_numeric <- c("rec", "time", "min_delay", "max_delay")
  }
  if (isTRUE(coords)) {
    accepted_numeric <- c("recX", "recY")
  }

  accepted_character <- c("tag_serial_no", "station_no")

  datetime_cols <- c("detection_timestamp_utc")

  bad_numeric <- check_present(df, accepted_numeric, is.numeric, "numeric")

  bad_character <- check_present(
    df,
    accepted_character,
    is.character,
    "character"
  )

  bad_datetime <- check_present(
    df,
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


#' @keywords internal
#' @rdname error_functions

check_data_frame <- function(df, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }

  if (!(inherits(df, c("data.frame", "tibble", "data.table")))) {
    cli::cli_abort(c(
      "`{arg_name}` must be a data.frame, tibble, or data.table",
      "i" = "Please provide data.frame"
    ))
  }
}

#' @param type is a `character` that is the type of delay desired.
#' @keywords internal
#' @rdname error_functions

check_delay <- function(vec, type, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (type == "custom" && is.null(vec)) {
    cli::cli_abort(
      "{arg_name} must be provided when {.arg type} is {.val {type}}."
    )
  }
}

#' @param draws is a `draws_df`object to be checked.
#' @keywords internal
#' @rdname error_functions

check_draws <- function(draws, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(draws))
  }

  if (!(inherits(draws, c("draws_df", "draws")))) {
    cli::cli_abort(c(
      "{.arg {arg_name}} must be a {.cls draws_df}, not {.cls {class(draws)}}",
      "i" = "Please provide a {.cls draws_df}, e.g. via {.fn posterior::as_draws_df}"
    ))
  }
}
#' @param draws_summary is a `draws_summary`object to be checked.
#' @keywords internal
#' @rdname error_functions

check_draw_summary <- function(draws_summary, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(draws_summary))
  }

  if (!(inherits(draws_summary, c("draws_summary")))) {
    cli::cli_abort(c(
      "{.arg {arg_name}} must be a {.cls draws_summary}, not {.cls {class(draws)}}",
      "i" = "Please provide a {.cls draws_summary}, e.g. via {.fn posterior::summarize_draws}"
    ))
  }
}

#' @param list is a `list` to be checked.
#' @keywords internal
#' @rdname error_functions

check_list <- function(list, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(list))
  }

  if (!(inherits(list, c("list")))) {
    cli::cli_abort(c(
      "`{arg_name}` must be a list",
      "i" = "Please provide a list"
    ))
  }
}

#' @param error `logical` value that dictates whether an error message is
#' displayed or a warning message. Default is `FALSE`.
#' @keywords internal
#' @rdname error_functions

check_lonlat <- function(sf, error = FALSE, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(sf))
  }
  crs <- sf::st_crs(sf)

  if (is.na(crs)) {
    cli::cli_abort(
      "{.arg {arg_name}} has no CRS set. Please set CRS using {.fnct {sf::st_crs()}}.",
      call = NULL
    )
  }

  is_longlat <- sf::st_is_longlat(sf)

  if (isTRUE(is_longlat)) {
    if (isFALSE(error)) {
      cli::cli_alert_warning(
        c(
          "{.arg {arg_name}} is currently in longlat degrees and is not projected, 
        making distance calculations inaccurate. Are you sure this is correct? ",
          "i" = "Current CRS: {.val {crs$input}} ",
          "i" = "To transform call {.code sf::st_transform({arg_name}, <projected_crs>)}."
        )
      )
    }

    if (isTRUE(error)) {
      cli::cli_abort(
        c(
          "{.arg {arg_name}} is currently in longlat degrees and is not projected making it
           not possible to create grid.",
          "i" = "Current CRS: {.val {crs$input}} ",
          "i" = "To transform call {.code sf::st_transform({arg_name}, <projected_crs>)}."
        )
      )
    }
  }

  invisible(sf)
}


#' @param arg_name_df the name of the argument of df to check.
#' @param arg_name_vec the name of the argument of vec to check.
#' @keywords internal
#' @rdname error_functions
check_nrec <- function(df, vec, arg_name_df = NULL, arg_name_vec = NULL) {
  if (is.null(arg_name_df)) {
    arg_name_df <- rlang::as_label(rlang::enexpr(df))
  }
  if (is.null(arg_name_vec)) {
    arg_name_vec <- rlang::as_label(rlang::enexpr(vec))
  }
  df_l <- length(unique(df$rec))

  if (!(vec >= df_l)) {
    cli::cli_abort(
      "`{arg_name_vec}` must be be equal to or greater than the number of receivers in {arg_name_df} "
    )
  }
}

#' @keywords internal
#' @rdname error_functions

check_numerical <- function(vec, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (!is.numeric(vec) || length(vec) != 1) {
    cli::cli_abort(c(
      "`{arg_name}` argument must be a numerical value.",
      "i" = "Please provide a numerical value"
    ))
  }
}

#' @keywords internal
#' @rdname error_functions

check_num_vec_len <- function(vec, vec_length = NULL, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (!is.numeric(vec) || !is.vector(vec) || length(vec) != vec_length) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric vector that has a length of {vec_length}."
    )
  }
}

#' @param cols is a character `vector` of column names to check
#' @param fnct is the name of a function to appply e.g., `is.numeric`.
#' @param label is the name of the group of cols e.g., `receiver`.
#' @rdname error_functions

check_present <- function(df, cols, fnct, label) {
  fnct <- match.fun(fnct)
  present <- intersect(cols, names(df))
  present[!vapply(df[present], fnct, logical(1))]
}


#' @keywords internal
#' @rdname error_functions

check_sf_object <- function(sf, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(sf))
  }

  if (!inherits(sf, "sf")) {
    cli::cli_abort(
      "`{arg_name}` must be a sf object (from {.pkg sf})."
    )
  }
}


#' @param stan is a `Stan` object.
#' @keywords internal
#' @name error_functions

check_stan_object <- function(stan, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(stan))
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

  if (!inherits(stan, valid_classes)) {
    cli::cli_abort(
      "`{arg_name}` must be a Stan object (from {.pkg rstan} or {.pkg cmdstanr})."
    )
  }
}

#' @keywords internal
#' @rdname error_functions

check_time <- function(df, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(df))
  }

  time_na <- is.na(df$detection_timestamp_utc)

  if (any(time_na)) {
    cli::cli_abort(c(
      "`{arg_name}` contains {sum(time_na)} missing value{?s}.",
      "i" = "`detection_timestamp_utc` cannot have values of {.field NA}"
    ))
  }
}


#' @keywords internal
#' @rdname error_functions

check_unit <- function(vec, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(vec))
  }

  if (!is.character(vec) || length(vec) != 1) {
    cli::cli_abort(c(
      "`{arg_name}` must be a single character string.",
      "i" = "e.g. \"1 hour\", \"15 minutes\", \"1 day\""
    ))
  }

  tryCatch(
    lubridate::floor_date(Sys.time(), unit = vec),
    error = function(e) {
      cli::cli_abort(c(
        "`{arg_name}` is not a valid {.fn lubridate::floor_date} unit: {.val {x}}",
        "i" = "e.g. \"1 hour\", \"15 minutes\", \"1 day\""
      ))
    }
  )

  invisible(vec)
}


#' Extract Draws
#'
#' These functions allow draws to be extracted from `{posterior}` objects.
#'
#' @param summary_draws a `draws_summary` object from `posterior::summarize_draws()`
#'
#' @details
#' `extract_coa()` - extracts median and the 2.5, and 97.5% quantiles for posterior draws of
#' `sx` and `sy`, which is the estiamted center of activity for a given individual within
#' a given time bin.
#'
#' @return `extract_coa()` - returns a `data.frame` containing
#' the median and the 2.5, and 97.5% quantiles.
#'
#' @keywords internal
#' @name extract_functions

extract_coa <- function(summary_draws) {
  check_draw_summary(summary_draws)

  coas_df <- summary_draws |>
    dplyr::filter(grepl("^s[xy]\\[", variable)) |>
    dplyr::mutate(
      coord = regmatches(variable, regexpr("^s[xy]", variable)),
      ind = as.integer(
        sub("^s[xy]\\[(\\d+),(\\d+)\\]$", "\\1", variable)
      ),
      time = as.integer(
        sub("^s[xy]\\[(\\d+),(\\d+)\\]$", "\\2", variable)
      )
    ) |>
    dplyr::select(ind, time, coord, median, q2.5, q97.5) |>
    tidyr::pivot_wider(
      names_from = coord,
      values_from = c(median, q2.5, q97.5),
      names_glue = "{coord}_{.value}"
    ) |>
    dplyr::rename(
      x = sx_median,
      x_lower = sx_q2.5,
      x_upper = sx_q97.5,
      y = sy_median,
      y_lower = sy_q2.5,
      y_upper = sy_q97.5
    ) |>
    dplyr::select(ind:y, x_lower, x_upper, y_lower, y_upper) |>
    dplyr::arrange(ind, time)

  return(coas_df)
}


#' @details
#' `extract_d_probs()` - extracts median and the 2.5, and 97.5% quantiles for posterior draws of
#' `p0` which is the detection probablity at distance 0, used when
#'  estimated in time varying and tag integrated models.
#'
#' @return `extract_d_probs()` - returns a `data.frame` containing
#' the median and the 2.5, and 97.5% quantiles.
#'
#' @keywords internal
#' @name extract_functions

extract_d_probs <- function(summary_draws) {
  check_draw_summary(summary_draws)

  d_probs <- summary_draws |>
    dplyr::filter(grepl("^p0\\[", variable)) |>
    dplyr::mutate(
      time = as.integer(sub("^p0\\[(\\d+),(\\d+)\\]$", "\\1", variable)),
      rec = as.integer(sub("^p0\\[(\\d+),(\\d+)\\]$", "\\2", variable))
    ) |>
    dplyr::select(time, rec, median, q2.5, q97.5)

  return(d_probs)
}
#' @param draws a `draws_df` object from `posterior::as_draws_df()`
#' @details
#' `extract_loc_draws()` - extracts posterior draws for the latent variables `sx` and `sy``
#' for each fish at each time bin from `draws_df` object and
#' transforms it so that the fish number, time, and draw are in a `data.frame`.
#' This can then be further ploted or transformed into a `sf` object.
#'
#' @return `extract_loc_draws()` - returns a `data.frame` containing the following columns:
#' `.chain`, `.iteration`, `.draw`, `lp__`, `fish`, `time`, `x`, and `y`.
#'
#' @keywords internal
#' @name extract_functions

extract_loc_draws <- function(draws) {
  check_draws(draws)

  loc_draws <- draws |>
    dplyr::as_tibble() |>
    dplyr::select(
      .chain,
      .iteration,
      .draw,
      lp__,
      dplyr::starts_with("sx["),
      dplyr::starts_with("sy[")
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("sx["), dplyr::starts_with("sy[")),
      names_to = c("coord", "fish", "time"),
      names_pattern = "(sx|sy)\\[(\\d+),(\\d+)\\]",
      names_transform = list(fish = as.integer, time = as.integer),
      values_to = "value"
    ) |>
    tidyr::pivot_wider(names_from = coord, values_from = value) |>
    dplyr::rename(x = sx, y = sy)
  return(loc_draws)
}

#' @details
#' `extract_param_draws()` - extracts posterior draws for the detection intercept (i.e., logit scale; `alpha0`),
#' the distance-decay coefficient (i.e., `alpha1`), any other coefficents, and the detction probablity at distance 0
#' (i.e. `p0`) from  `draws_df` object.
#'
#' @return `extract_param_draws()` - returns a `data.frame` containing the following columns:
#' `.chain`, `.iteration`, `.draw`, `lp__` and then posterior draws for the paramaters of
#' the detection probablity likihood (i.e., `alpha0` and `alpha1`) and generated quantiteies (i.e., `p0`).
#'
#' @keywords internal
#' @name extract_functions

extract_param_draws <- function(draws) {
  check_draws(draws)

  param_draws <- draws |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("sx["), dplyr::starts_with("sy[")),
      names_to = c("coord", "fish", "time"),
      names_pattern = "(sx|sy)\\[(\\d+),(\\d+)\\]",
      names_transform = list(fish = as.integer, time = as.integer),
      values_to = "value"
    ) |>
    dplyr::select(-coord, -value) |>
    dplyr::relocate(
      .chain,
      .iteration,
      .draw,
      lp__,
      fish,
      time
    )

  return(param_draws)
}


#' Summarize Posterior Draws
#'
#' @param draws a `draws_df` object from `posterior::as_draws_df()`
#'
#' @return returns a summarized dataframe with the median and 2.5% and 97.5% quantitles.
#'
#' @keywords internal
#' @name summarize_functions

summarize_draws <- function(draws) {
  check_draws(draws)

  sum_draws <- posterior::summarise_draws(
    draws,
    median = stats::median,
    ~ stats::quantile(.x, probs = c(0.025, 0.975))
  ) |>
    dplyr::rename(
      q2.5 = `2.5%`,
      q97.5 = `97.5%`
    )
  return(sum_draws)
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

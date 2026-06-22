# ----- Build aeqd -----

#' Build Functions
#'
#' These functions structure and build data components that are needed by
#' the model. Each function will either return an object that
#' is properly built and structured for `Stan`` or will produce an object
#' to be latter used in the pre-process phase of analysis.
#'
#' @param array_sf the receiver array as an `sf` object.
#'
#' @details
#' `build_aeqd()`- Azimuthal Equidistant projection is needed by the model
#' which relies on creating a centroid and creating equal distances from the
#' centroid. This function quickly creates the project string
#' needed to transform an exisiting crs to Azimuthal Equidistant projection.
#'
#' @return `build_aeqd()`- retruns a `vector` containing the site specific projection string for
#' the array to be able to transform the crs.
#'
#' @name build_functions
#' @export

build_aeqd <- function(array_sf) {
  check_sf_object(array_sf)
  check_utm(array_sf)
  center <- sf::st_centroid(sf::st_union(array_sf))

  array_crs <- sf::st_crs(array_sf)

  if (!(array_crs$input %in% "EPSG:4326")) {
    center_dd <- center |>
      sf::st_transform(4326)
  } else {
    center_dd <- center
  }

  coords_extract <- sf::st_coordinates(center_dd)

  aeqd_crs <- sprintf(
    "+proj=aeqd +lon_0=%f +lat_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=km",
    coords_extract[, "X"],
    coords_extract[, "Y"]
  )
  cli::cli_alert_success(
    "Successfully built {.val {aeqd_crs}}"
  )
  return(aeqd_crs)
}

# ---- build bbox ----
#' @param coord_df a `data.frame` that contains two columns named `recX` and `recY`
#' created by `build_rec_coords()`.
#' @param buffer a `numerical` value to set the buffer. Defaults to `1`. Considering the
#' default Azimuthal Equidistant projection is km, 1 represents a 1 km buffer.
#'
#' @details
#' `build_bbox()`- builds the boundary box of receiver array needed by the model.
#'
#' @return `build_bbox()`- returns a `data.frame` containing two columns named `xlim` and `ylim` which are the
#' minimum and maximum values +/- a buffer for x and y.
#'
#' @name build_functions
#' @export

build_bbox <- function(coord_df, buffer = NULL) {
  check_data_frame(coord_df)
  check_column_names(coord_df, coords = TRUE)
  check_column_type(coord_df, coords = TRUE)

  if (is.null(buffer)) {
    buffer <- 1
  }
  check_numerical(buffer)

  xlim <- c(min(coord_df$recX - buffer), max(coord_df$recX + buffer))
  ylim <- c(min(coord_df$recY - buffer), max(coord_df$recY + buffer))

  coord_limit <- data.frame(
    xlim = xlim,
    ylim = ylim
  )

  return(coord_limit)
}
# ---- 3-dimensional Count Array ----

#' @param df a `data.frame` that contains the following column names
#' `tag_serial_no`, `rec`, and `time`. The column `time` is an index of
#' `time_bin`, while `rec` is an index of the `station_no`.
#' @param nrec a `numerical` value that is number of receivers in the telemetry array.
#' @param rec_id a `vector` that matches the length of `nrec` and needs to be index of the
#' receivers in the telemetry array.
#' @param rec_names an optional `vector` that contains the station names of the receivers. If not
#' supplied it will default to using `rec_id`
#'
#' @details
#' `build_counts()` - builds a 3-dimensional count `array`. The models need the
#' counts at each recevier for each time bin for each individual structure in 3-dimensional `array`.
#' This functions takes in a detection `data.frame`, creates a count `data.frame` and then transforms it
#' into a 3-dimensional `array` that will be pased to the `Stan` model.
#'
#'
#' @return `build_counts()` - returns a 3-dimensional `array` containing the number of detections for the following dimensions,
#' the number of invividuals, by the number of time bins, by the number of receivers.
#'
#' @name build_functions
#' @export

build_counts <- function(df, nrec, rec_id, rec_names = NULL) {
  check_data_frame(df)
  check_column_names(df)
  check_column_type(df)
  check_numerical(nrec)
  check_nrec(df, nrec)

  check_num_vec_len(rec_id, vec_length = nrec)

  if (!is.null(rec_names)) {
    check_char_vec_len(rec_names, vec_length = nrec)
  }

  df_count <- df |>
    # Aggregate the number of detections for each individual at each receiver
    # in each time step maybe add station_no
    dplyr::count(tag_serial_no, rec, time) |>
    # Create a numeric identifier for each transmitter
    dplyr::mutate(tag = as.numeric(as.factor(tag_serial_no)))

  # wee need to make tag and rec ids
  tag_id <- sort(unique(df_count$tag_serial_no))
  time_id <- sort(unique(df_count$time))

  nind <- length(unique(df_count$tag))

  # we then need to assign these back to df_count to go over
  df_count <- df_count |>
    dplyr::mutate(
      tag_idx = match(tag_serial_no, tag_id),
      rec_idx = match(rec, rec_id),
      time_idx = match(time, time_id)
    )

  # get number of invivdiausl and timestps
  tsteps <- length(time_id)

  if (!is.null(rec_names)) {
    rec_names
  } else {
    rec_names <- rec_id
  }

  # now we can
  Y <- array(
    0L,
    dim = c(nind, tsteps, nrec),
    dimnames = list(
      ind = tag_id,
      time = time_id,
      rec = rec_names
    )
  )

  # Explicitly st each dex column to integer — avoidsilent double coercion
  idx <- matrix(
    c(
      as.integer(df_count$tag_idx),
      as.integer(df_count$time_idx),
      as.integer(df_count$rec_idx)
    ),
    # as.integer(df$time)),
    ncol = 3
  )

  Y[idx] <- as.integer(df_count$n)

  return(Y)
}

# ---- build intials ------
#' @param coord_df a `data.frame` that contains two columns named `recX` and `recY`
#' created by `build_rec_coords()`.
#' @param nind a `numerical` value to the number of individuals.
#' @param tstep a `numerical` value to the number of time steps.
#'
#' @details
#' `build_init()`- builds the initial values to supply to the model. Supplying these values
#' can helps the model know where to intially start.
#'
#' @return `build_init()`- returns a `list` containing  two matrices named `sx` and `sy`. Each
#' matrix is the mean of either `recX` or `recY` produced by `build_rec_coords()` by the
#' number of individuals by the number of timesteps.
#' @name build_functions
#' @export

build_init <- function(coord_df, nind, tstep) {
  check_data_frame(coord_df)
  check_column_names(coord_df, coords = TRUE)
  check_column_type(coord_df, coords = TRUE)
  check_numerical(nind)
  check_numerical(tstep)

  out <- list(
    sx = matrix(
      mean(coord_df$recX),
      nrow = nind,
      ncol = tstep
    ),
    sy = matrix(
      mean(coord_df$recY),
      nrow = nind,
      ncol = tstep
    )
  )
  return(out)
}
# ---- build ntrans ------

#' @param df a `data.frame` that contains the following column names
#' `tag_serial_no`, `rec`, `time`, `min_delay`, `max_delay`. The column `time` is an index of
#' `time_bin`, while `rec` is an index of the `station_no`.
#' @param type can either be `mean`, `min`, `max`, or `custom`. This will change how the number of
#' transmissions that are expected within a time bin are calculated. When set to `mean` (the deafult), the
#' function will calculate the mean time delay (s) between the minimumm and maximum delay (s),
#' while `min` and `max`, will use the minimumm or maximum delay value, respectively. If
#' specifying `custom` (see `custom_delay`), the user can enter the delay of their choosing.
#' Adjusting the delays used can useful becaue the number of
#' detections within a time bin can exceed the number of transmissions expected wtihin the time bin which
#' will cause the model to fail.
#' @param custom_delay Only needed when `type` is set to `"custom"`. When supplied, this argument, which is
#' a `numeric` will allow a custom value to be used for the tag delays.
#'
#' @details
#'
#' `build_ntrans()` - builds the nubmer of transmissions to be expected within a given time bin.
#'
#' @return `build_ntrans()` - retruns a single value vector.
#'
#' @name build_functions
#' @export

build_ntrans <- function(
  df,
  type = c("mean", "min", "max", "custom"),
  custom_delay = NULL
) {
  check_data_frame(df)
  check_column_names(df)
  check_column_type(df)
  if (!(is.null(custom_delay))) {
    check_numerical(custom_delay)
  }

  type <- match.arg(type)

  check_delay(vec = custom_delay, type = type, arg_name = "custom_delay")

  bin_secs <- df |>
    dplyr::distinct(time_bin) |>
    dplyr::arrange(time_bin) |>
    dplyr::mutate(
      bin_secs = as.numeric(dplyr::lead(time_bin) - time_bin, units = "secs")
    ) |>
    tidyr::fill(bin_secs, .direction = "down")

  bin_label <- bin_secs |>
    dplyr::mutate(
      bin_label = dplyr::case_when(
        bin_secs %% 86400 == 0 ~ paste(bin_secs / 86400, "day(s)"),
        bin_secs %% 3600 == 0 ~ paste(bin_secs / 3600, "hour(s)"),
        bin_secs %% 60 == 0 ~ paste(bin_secs / 60, "minute(s)"),
        TRUE ~ paste(bin_secs, "second(s)")
      )
    ) |>
    dplyr::pull(bin_label) |>
    unique()

  delay_col <- switch(
    type,
    mean = "mean_delay",
    min = "min_delay",
    max = "max_delay",
    custom = "custom_delay"
  )
  # check_delay(custom_delay)

  ntrans <- df |>
    dplyr::left_join(bin_secs, by = "time_bin") |>
    dplyr::mutate(
      mean_delay = (min_delay + max_delay) / 2,
      custom_delay = custom_delay,
      ntrans = floor(bin_secs / .data[[delay_col]])
    ) |>
    dplyr::pull(ntrans) |>
    unique()

  cli::cli_alert_success(
    "Successfully built the number of transmission {.val {ntrans}} expectd in {.val {bin_label}} bins based off of
    {.val {paste(type, 'delay')}}."
  )
  return(ntrans)
}


# ----- Pixel Grid -----

#' @param bnd_sf a `sf` object that is boundary that is desired to impose
#' @param res the resolution desired.
#' @param crs the Azimuthal Equidistant projection desired. If the supplied
#' projection string does not contain `"+proj=aeqd"`. The function will error.
#'
#' @details
#' `build_pixel_grid()` - builds barrier grid for the model.
#' We need to convert the boundary into pixels that can be used to recongize where to estimate
#' detection probablity. The boundary that is supplied needs to be in UTMs, with the returned
#' values being in Azimuthal Equidistant projection. To build this projection see `build_aeqd()`.
#' When supplying the desired resolution remember that this is in m so
#' a value of `1` would be quite small while of `1000` is 1 km which makes a less dense grid.
#'
#'
#' @return `build_pixel_grid()` - returns a `list` contain the the number of pixels `n_pixel`, the pixel x coordinates
#' (`pix_x`) and the pixel y coordinates (`pix_y`).
#'
#' @name build_functions
#' @export

build_pixel_grid <- function(bnd_sf, res, crs) {
  check_sf_object(bnd_sf)
  check_utm(bnd_sf)
  check_aeqd_string(crs)
  check_numerical(res)

  coords <- bnd_sf |>
    sf::st_bbox() |>
    sf::st_make_grid(cellsize = res, what = "centers") |>
    sf::st_as_sf() |>
    sf::st_filter(bnd_sf) |>
    sf::st_transform(crs) |>
    sf::st_coordinates()

  out <- list(
    n_pixels = nrow(coords),
    pix_x = coords[, "X"],
    pix_y = coords[, "Y"]
  )
  return(out)
}

# ----- build_rec_coords ----

#' @param obj_sf a `sf` object that the receiver locations as `POINT` geometry.
#' The `sf` object has to be in Azimuthal Equidistant projection.
#'
#' @details
#' `build_rec_coords()` - builds receiver coordinates as the models need the
#' easting and northing (i.e., x and y) coordinates of the receivers.
#'
#' @return `build_rec_coords()` - a `data.frame` containing two `columns` named `recX` and `recY` which are the
#' receiver locations transformed into Azimuthal Equidistant projection.
#'
#' @name build_functions
#' @export

build_rec_coords <- function(obj_sf) {
  check_sf_object(obj_sf)
  check_aeqd(obj_sf, "obj_sf")

  rec <- sf::st_coordinates(obj_sf, geometry)

  # ---- could return x and y as vector in a list ----
  coord_df <- data.frame(
    recX = rec[, "X"],
    recY = rec[, "Y"]
  )

  return(coord_df)
}
# ---- build time bins ----

#' @param df a `data.frame` that contains the following column names
#' `tag_serial_no`
#' @param unit a `character` that is the unit desired to bin. Default is `"1 hour"`.
#' See `lubridate::floor_date()`'s `unit` argument for more details.
#'
#' @details
#' `build_time_bin()`- builds and adds time bins to the detection `data.frame`.
#'
#' @return `build_time_bin()`- returns a `data.frame` that has had the columns `time_bin` and `time`added. `time` is an
#' index value of `time_bin` and is needed by the model. This will further be used by `build_counts()`.
#'
#' @name build_functions
#' @export

build_time_bin <- function(df, unit = "1 hour") {
  check_data_frame(df)
  check_column_names(df)
  check_column_type(df)
  check_time(df)
  check_unit(unit)

  df <- df |>
    dplyr::arrange(detection_timestamp_utc) |>
    dplyr::mutate(
      time_bin = lubridate::floor_date(detection_timestamp_utc, unit = unit),
      time = dplyr::dense_rank(time_bin)
    )

  return(df)
}

# ----- Time steps -----

#' @param x a 3-dimensional count array.
#' @details
#' `build_tstep()` - builds the number of total time steps that exist whithin the
#' supplied 3-dimensional count array.
#' @return `build_tstep()` - a numerical value that is the number of timesteps.
#'
#' @name build_functions
#' @export

build_tstep <- function(x) {
  check_array(x)
  tstep <- dim(x)[2]
  cli::cli_alert_success(
    "Successfully built the number of time steps {.val {tstep}}"
  )
  return(tstep)
}

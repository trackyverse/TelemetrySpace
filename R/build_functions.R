#' Build Functions
#'
#' These functions all build key data compoents that are needed to
#' properly structure the data to be submitted to Stan.
#'
#' @param array_sf the receiver array as an `sf` object.
#'
#' @details
#' Build Azimuthal Equidistant Projection - To make computations in Telemetryspace easier
#' Azimuthal Equidistant projection is used which relies
#' on creating a centroid and creating equal distances from that
#' centroid. This function quickly creates the project string
#' needed to supply a `sf` function the crs.
#'
#' @return a `vector` containing the site specific projection string for
#' the array to be able to supply `sf` functions with a valid
#' crs to transform values into.
#'
#'
#' @name build_functions
#' @export
#'

build_aeqd <- function(array_sf) {
  check_sf_object(array_sf, "array_sf")
  centre <- sf::st_centroid(sf::st_union(array_sf))

  array_crs <- sf::st_crs(array_sf)

  if (!(array_crs$input %in% "EPSG:4326")) {
    center_dd <- centre |>
      sf::st_transform(4326)
  }

  clon <- sf::st_coordinates(center_dd)[, "X"]
  clat <- sf::st_coordinates(center_dd)[, "Y"]

  aeqd_crs <- sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=km",
    clat,
    clon
  )
  cli::cli_alert_success(
    "Successfully created {.val {aeqd_crs}}"
  )
  return(aeqd_crs)
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
#' Build 3-dimensional Count Array - Prior to running the model, detection data needs to be
#' transformed into the number of
#' counts at each recevier for each time bin for each individual. This functions
#' takes in a detection `data.frame`, creates a count `data.frame` and then transforms it
#'  into a 3-dimensional `array` that will be pased to the `Stan` model.
#'
#'
#' @return a 3-dimensional `array` containing the number of detections for the following dimensions
#' the number of invividuals, by the number of time bins, by the number of receivers.
#'
#' @name build_functions
#' @export
#'

build_counts <- function(df, nrec, rec_id, rec_names = NULL) {
  check_data_frame(df, arg_name = "df")
  check_column_names(df, arg_name = "df")
  check_column_type(df, arg_name = "df")
  check_numerical(nrec, arg_name = "nrec")
  check_num_vec_len(rec_id, vec_length = nrec, arg_name = rec_id)

  if (!is.null(rec_names)) {
    check_char_vec_len(rec_names, "rec_names", vec_length = nrec)
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
# ---- build ntrans ------

#' @param df a `data.frame` that contains the following column names
#' `tag_serial_no`, `rec`, and `time`. The column `time` is an index of
#' `time_bin`, while `rec` is an index of the `station_no`.
#'
#' @details
#'
#' Build Nubmer of Transmissions - Build the number of transmissions to be expected within a given time bin.
#'
#' @return a single value vector.
#'
#' @name build_functions
#' @export
build_ntrans <- function(df) {
  check_data_frame(df)

  bin_secs <- df |>
    dplyr::distinct(time_bin) |>
    dplyr::arrange(time_bin) |>
    dplyr::mutate(
      bin_secs = as.numeric(dplyr::lead(time_bin) - time_bin, units = "secs")
    ) |>
    tidyr::fill(bin_secs, .direction = "down")

  ntrans <- df |>
    dplyr::left_join(bin_secs, by = "time_bin") |>
    dplyr::mutate(
      mean_delay = (min_delay + max_delay) / 2,
      ntrans = floor(bin_secs / mean_delay)
    ) |>
    dplyr::pull(ntrans) |>
    unique()
  return(ntrans)
}


# ----- Pixel Grid -----

#' @param bnd_sf a `sf` object that is boundary that is desired to impose
#' @param res the resolution desired
#'
#' @details the boundary that is supplied needs to match the crs of the receivers.
#' Often an azimuth eqaul distance projection is used for TelemetrySpace. This
#' projection is in kilometer (km). To build this
#' project see `build_aeqd()` function. When supplying the desired resolution
#' remember that this is in km so a value of `1`` would be quite large while a value
#' of `0.1` is 100 m which makes a much more dense grid.
#'
#'
#' @details
#' Build Pixel Grid - To make a barrier for the model, we need to convert the boundary into
#' pixels that we can use to for the model to recongize where to estimate
#' detection probablity.
#'
#' @return a `list` contain the the number of pixels `n_pixel`, the pixel x coordinates
#' (`pix_x`) and the pixel y coordinates (`pix_y`).
#'
#' @name build_functions
#'
#' @export

build_pixel_grid <- function(bnd_sf, res) {
  check_sf_object(bnd_sf, "bnd_sf")

  # get boundary box of boundary
  bbox <- sf::st_bbox(bnd_sf)

  # Build centroid grid over bounding box
  gx <- seq(bbox["xmin"] + res / 2, bbox["xmax"], by = res)
  gy <- seq(bbox["ymin"] + res / 2, bbox["ymax"], by = res)

  # expand x and y to make grid over bondary
  pts <- expand.grid(x = gx, y = gy)
  # convert to sf object
  pts_sf <- pts |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(bnd_sf))

  # Keep only pixels whose centroid falls inside the lake polygon
  inside <- sf::st_filter(pts_sf, bnd_sf)
  coords <- sf::st_coordinates(inside)

  out <- list(
    n_pixels = nrow(coords),
    pix_x = coords[, "X"],
    pix_y = coords[, "Y"]
  )
  return(out)
}

# ----- Pixel Grid -----

#' @param obj_sf a `sf` object that the receiver locations as `sf` `POINT` object.
#' The `sf` object has to be in Azimuthal Equidistant projection.
#'
#' @details
#' Build Receiver Coordinates - The models need the easting and northing (i.e., x and y) coordinates of the receivers.
#' This function takes a sf object and returns a `list` that contains the
#' easting and northing coordinates in Azimuthal Equidistant Projection.
#'
#' @return a `list` conttaining two `vectors` named `recX` and `recY` which are the
#' receiver locations transformed into Azimuthal Equidistant projection.
#'
#' @name build_functions
#'
#' @export

build_rec_coords <- function(obj_sf) {
  check_sf_object(obj_sf, "obj_sf")
  check_aeqd(obj_sf)

  recX <- sf::st_coordinates(obj_sf, geometry)[, "X"]
  recY <- sf::st_coordinates(obj_sf, geometry)[, "Y"]

  # ---- could return x and y as vector in a list ----
  coord_list <- list(
    recX = recX,
    recY = recY
  )

  return(coord_list)
}

#' @param coord_list a `list` object that contains two `vectors` named `recX` and `recY`
#' created by `build_rec_coords`.
#' @param buffer a `numerical` value to set the buffer. Defaults to `1`. Considering the
#' default Azimuthal Equidistant projection is km, 1 represents a 1 km buffer.
#'
#' @details
#' Build Coordinate Limits - The models need the limits of easting and northing (i.e., x and y) coordinates of the
#' receiver array. This can viewed as the boundary box.
#'
#'
#'
#' @return a `list` conttaining two `vectors` named `xlim` and `ylim` which are the
#' minimum and maximum values +/- a buffer for x and y.
#'
#' @name build_functions
#'
#' @export

build_rec_limits <- function(coord_list, buffer = NULL) {
  if (is.null(buffer)) {
    buffer <- 1
  }
  check_numerical(buffer)
  check_list(coord_list)

  xlim <- c(min(coord_list$recX - buffer), max(coord_list$recX + buffer))
  ylim <- c(min(coord_list$recY - buffer), max(coord_list$recY + buffer))

  coord_limit <- list(
    xlim = xlim,
    ylim = ylim
  )

  return(coord_limit)
}

build_time_bin <- function(x, unit = NULL) {
  check_data_frame(x)

  if (is.null(unit)) {
    unit <- "1 hour"
  }
  check_unit(unit)

  x <- x |>
    dplyr::arrange(detection_timestamp_est) |>
    dplyr::mutate(
      time_bin = lubridate::floor_date(detection_timestamp_est, unit = unit),
      time = dplyr::dense_rank(time_bin)
    )

  return(x)
}

# ----- Time steps -----

#' @param x a 3-dimensional count array.
#' @details
#' Build Time Steps - This function builds the number of total time steps that exist whithin the
#' supplied 3-dimensional count array.
#'
#'
#'
#' @return a numerical value that is the number of timesteps
#' @name build_functions
#'
#' @export

build_tstep <- function(x) {
  check_array(x, "x")
  tstep <- dim(x)[2]
  return(tstep)
}

# ----- lets setup using new data -----
# transform to utms

# ps_rec_loc <- ps_rec_loc |>
#   sf::st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)

# ps_rec_loc_utm <- ps_rec_loc |>
#   sf::st_transform(32617)

# # ---- build aeqd ------
# aeqd_crs <- build_aeqd(ps_rec_loc_utm)
# # transform to aeqd
# ps_rec_loc_aeqd <- ps_rec_loc |>
#   sf::st_transform(aeqd_crs) |>
#   (\(.) .[order(.$station_no), ])()
# # ---- index the receivers -----

# ps_rec_loc_aeqd
# ps_rec_loc_aeqd$rec <- 1:nrow(ps_rec_loc_aeqd)

# # build receiver vectors
# rec_loc_vec <- build_rec_coords(ps_rec_loc_aeqd)

# # build receiver limits
# rec_limits <- build_bbox(rec_loc_vec)

# # ----- build time bins -----
# ps_det_example_clean <- ps_det_example
# ps_det_example <- build_time_bin(ps_det_example, unit = "1 hour")
# ps_det_example_t <- build_time_bin(ps_det_example, unit = "1 hour")

# ps_det_example <- ps_det_example[ps_det_example$time < 3, ]

# # ----- merge receiver index with detection info -----

# ps_det_example <- merge(
#   ps_det_example,
#   sf::st_drop_geometry(ps_rec_loc_aeqd),
#   by = "station_no"
# )
# unique(ps_det_example$station_no)

# # ----- build counts ------
# ps_count_example <- build_counts(
#   df = ps_det_example,
#   nrec = nrow(ps_rec_loc_aeqd),
#   rec_id = ps_rec_loc_aeqd$rec,
#   rec_names = ps_rec_loc_aeqd$station_no
# )
# # ----- build timesteps -------
# time_steps <- build_tstep(ps_count_example)

# # ----- build nind and ntrans -----
# nind <- length(unique(ps_det_example$tag_serial_no))

# ntrans <- build_ntrans(ps_det_example)

# ------ test build aeqd ------
test_that("build_aeqd returns a valid AEQD proj string from UTM input", {
  # ps_rec_loc is EPSG:4326; workflow transforms to UTM first

  result <- build_aeqd(ps_rec_loc_utm)

  expect_type(result, "character")
  expect_match(result, "\\+proj=aeqd")
  expect_match(result, "\\+lat_0=")
  expect_match(result, "\\+lon_0=")
  expect_match(result, "\\+units=km")
})

test_that("build_aeqd lon/lat values are in plausible Parry Sound range", {
  result <- build_aeqd(ps_rec_loc_utm)

  lat <- as.numeric(
    regmatches(
      result,
      regexpr("(?<=\\+lat_0=)-?[0-9.]+", result, perl = TRUE)
    )
  )
  lon <- as.numeric(
    regmatches(
      result,
      regexpr("(?<=\\+lon_0=)-?[0-9.]+", result, perl = TRUE)
    )
  )

  expect_true(lat > 44 && lat < 46, label = "lat_0 in Parry Sound range")
  expect_true(lon > -81 && lon < -79, label = "lon_0 in Parry Sound range")
})

test_that("build_aeqd result is usable as a CRS for st_transform", {
  aeqd <- build_aeqd(ps_rec_loc_utm)

  expect_no_error(sf::st_transform(ps_rec_loc, aeqd))
  transformed <- sf::st_transform(ps_rec_loc, aeqd)
  expect_s3_class(transformed, "sf")
})

test_that("build_aeqd errors on non-sf input", {
  expect_error(build_aeqd(data.frame(x = 1, y = 2)))
  expect_error(build_aeqd("not_sf"))
})

# ---- test build_rec_coords -----

test_that("build_rec_coords returns correct coordinate data frame", {
  rec_loc_vec_f <- rec_loc_vec[1:2, ]

  expect_s3_class(rec_loc_vec_f, "data.frame")
  expect_named(rec_loc_vec_f, c("recX", "recY"))
  expect_equal(nrow(rec_loc_vec_f), 2L)
  expect_equal(rec_loc_vec_f$recX, c(-1.113863, -1.097270), tolerance = 1e-5)
  expect_equal(rec_loc_vec_f$recY, c(-1.543388, -0.810991), tolerance = 1e-5)
})

test_that("build_rec_coords fails on non-sf input", {
  aeqd_df <- data.frame(x = 1:3, y = 1:3)
  expect_error(build_rec_coords(aeqd_df))
})

test_that("build_rec_coords fails on wrong CRS", {
  ps_error <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(c(-80, 45)),
      crs = 4326
    )
  )
  expect_error(build_rec_coords(ps_error))
})

# ------ build_rec_limits -------

test_that("returns a data.frame with xlim and ylim columns", {
  expect_s3_class(rec_limits, "data.frame")
  expect_named(rec_limits, c("xlim", "ylim"))
  expect_equal(nrow(rec_limits), 2L)
})

test_that("default buffer of 1 is applied correctly", {
  expect_equal(rec_limits$xlim, c(-4.496181, 3.107017), tolerance = 0.0001)
  expect_equal(rec_limits$ylim, c(-3.282735, 4.189363), tolerance = 0.0001)
})

test_that("custom buffer is applied correctly", {
  df <- data.frame(recX = c(0, 10), recY = c(0, 20))
  rec_limits_b <- build_bbox(rec_loc_vec, 2)
  expect_equal(rec_limits_b$xlim, c(-5.49618, 4.10701), tolerance = 0.0001)
  expect_equal(rec_limits_b$ylim, c(-4.28273, 5.18936), tolerance = 0.0001)
})

test_that("errors on non-data.frame input", {
  expect_error(build_bbox(matrix(1:4, 2, 2)))
  expect_error(build_bbox(list(recX = 1:3, recY = 1:3)))
})

test_that("errors on non-numeric buffer", {
  df <- data.frame(recX = 1:3, recY = 1:3)
  expect_error(build_bbox(df, buffer = "large"))
  expect_error(build_bbox(df, buffer = TRUE)) #
})

# ------ build_time_bin -----

test_that("build_time_bin returns a data frame with expected columns", {
  result_t <- build_time_bin(ps_det_example_clean)
  expect_s3_class(result_t, "data.frame")
  expect_true(all(c("time_bin", "time") %in% names(result_t)))
  expect_equal(nrow(result_t), nrow(ps_det_example_clean))
})

test_that("build_time_bin defaults to 1 hour bins when unit is NULL", {
  result_explicit <- build_time_bin(ps_det_example_clean, unit = "1 hour")

  expect_equal(ps_det_example_t$time_bin, result_explicit$time_bin)
  expect_equal(ps_det_example_t$time, result_explicit$time)
})

test_that("build_time_bin floors timestamps to the specified unit", {
  result <- build_time_bin(ps_det_example, unit = "2 hour")

  expect_equal(
    result$time_bin,
    lubridate::floor_date(result$detection_timestamp_utc, unit = "2 hour")
  )
  # every bin should be exactly on the hour
  expect_true(all(lubridate::minute(result$time_bin) == 0))
  expect_true(all(lubridate::second(result$time_bin) == 0))
})

test_that("build_time_bin respects a finer unit (e.g. 10 minutes)", {
  result <- build_time_bin(ps_det_example, unit = "10 minutes")

  expect_equal(
    result$time_bin,
    lubridate::floor_date(result$detection_timestamp_utc, unit = "10 minutes")
  )
  expect_true(all(lubridate::minute(result$time_bin) %% 10 == 0))
})

test_that("build_time_bin assigns dense-ranked integer time values starting at 1", {
  result <- build_time_bin(ps_det_example)

  expect_type(result$time, "integer")
  expect_equal(min(result$time), 1)
  # number of distinct ranks must equal number of distinct bins
  expect_equal(length(unique(result$time)), length(unique(result$time_bin)))
  # rank order must be non-decreasing for increasing time_bin
  ordered <- result[order(result$time_bin), ]
  expect_true(all(diff(ordered$time) >= 0))
})

test_that("build_time_bin output is sorted by detection_timestamp_utc", {
  result <- build_time_bin(ps_det_example)

  expect_true(all(diff(as.numeric(result$detection_timestamp_utc)) >= 0))
})

test_that("build_time_bin assigns the same time_bin/time to rows in the same bin", {
  result <- build_time_bin(ps_det_example, unit = "1 day")

  dup_check <- result |>
    dplyr::group_by(time_bin) |>
    dplyr::summarise(
      n_distinct_time = dplyr::n_distinct(time),
      .groups = "drop"
    )

  expect_true(all(dup_check$n_distinct_time == 1))
})

test_that("build_time_bin errors on invalid unit strings", {
  expect_error(build_time_bin(ps_det_example, unit = "huor"))
})

test_that("build_time_bin errors when input is not a data frame", {
  expect_error(build_time_bin(list(detection_timestamp_utc = Sys.time())))
  expect_error(build_time_bin(matrix(1:4, nrow = 2)))
})

test_that("build_time_bin errors when required columns are missing", {
  incomplete_df <- ps_det_example |>
    dplyr::select(-detection_timestamp_utc)

  expect_error(build_time_bin(incomplete_df))
})

test_that("build_time_bin errors when detection_timestamp_utc has wrong type", {
  bad_type_df <- ps_det_example |>
    dplyr::mutate(
      detection_timestamp_utc = as.character(detection_timestamp_utc)
    )

  expect_error(build_time_bin(bad_type_df))
})

test_that("build_time_bin errors on NA detection_timestamp_utc values", {
  na_df <- ps_det_example
  na_df$detection_timestamp_utc[1] <- NA

  expect_error(build_time_bin(na_df))
})

# ----- build_counts ----

test_that("build_counts returns an array with correct dimensions and dimnames", {
  expect_true(is.array(ps_count_example))
  expect_equal(dim(ps_count_example), c(1L, 2L, 80L))
  expect_equal(dimnames(ps_count_example)$ind, c("1594061"))
  expect_equal(dimnames(ps_count_example)$time, c("1", "2"))
  expect_equal(dimnames(ps_count_example)$rec, ps_rec_loc_aeqd$station_no)
})

test_that("build_counts correctly aggregates detection counts per tag/rec/time", {
  expect_equal(ps_count_example["1594061", "1", "PSM-003"], 3L)
  expect_equal(ps_count_example["1594061", "1", "PSM-002"], 0L)
})


test_that("build_counts uses rec_id as dimnames when rec_names is NULL", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  rec_id <- c(101, 102)

  Y <- build_counts(df = df, nrec = 2, rec_id = rec_id, rec_names = NULL)

  expect_equal(dimnames(Y)$rec, as.character(rec_id))
})

test_that("build_counts uses rec_names for dimnames when provided", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  rec_id <- c(101, 102)
  rec_names <- c("station_a", "station_b")

  Y <- build_counts(
    df = df,
    nrec = 2,
    rec_id = rec_id,
    rec_names = rec_names
  )

  expect_equal(dimnames(Y)$rec, rec_names)
  # underlying rec_id is still used for matching, not rec_names
  expect_equal(Y["A1", "1", "station_a"], 1L)
})

test_that("build_counts errors when rec_names length does not match nrec", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  rec_id <- c(101, 102)

  expect_error(
    build_counts(
      df = df,
      nrec = 2,
      rec_id = rec_id,
      rec_names = c("station_a")
    )
  )
})

test_that("build_counts errors when rec_id length does not match nrec", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  expect_error(
    build_counts(
      df = df,
      nrec = 2,
      rec_id = c(101) # length 1, should be 2
    )
  )
})

test_that("build_counts errors when nrec is not numeric", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  expect_error(
    build_counts(df = df, nrec = "two", rec_id = c(101, 102))
  )
})

test_that("build_counts errors when df is missing required columns", {
  df_missing_rec <- data.frame(
    tag_serial_no = c("A1"),
    time = c(1L)
  )

  expect_error(
    build_counts(df = df_missing_rec, nrec = 2, rec_id = c(101, 102))
  )
})

test_that("build_counts errors when df is not a data.frame", {
  expect_error(
    build_counts(
      df = list(tag_serial_no = "A1", rec = 101, time = 1L),
      nrec = 2,
      rec_id = c(101, 102)
    )
  )
})

test_that("build_counts handles a single individual, single receiver, single time step", {
  df <- data.frame(
    tag_serial_no = c("A1", "A1", "A1"),
    rec = c(101, 101, 101),
    time = c(1L, 1L, 1L)
  )

  Y <- build_counts(df = df, nrec = 1, rec_id = c(101))

  expect_equal(dim(Y), c(1L, 1L, 1L))
  expect_equal(Y[1, 1, 1], 3L)
})

test_that("build_counts preserves integer type in the output array", {
  df <- data.frame(
    tag_serial_no = c("A1"),
    rec = c(101),
    time = c(1L)
  )

  Y <- build_counts(df = df, nrec = 1, rec_id = c(101))

  expect_type(Y, "integer")
})


# ----- build_tstep() ------

test_that("build_tstep returns the correct number of time steps for a 3D array", {
  expect_equal(time_steps, 2)
})

test_that("build_tstep handles a single time step", {
  x <- array(1:3, dim = c(3, 1, 1))
  expect_equal(build_tstep(x), 1)
})

test_that("build_tstep returns an integer-like value, not a list or array", {
  x <- array(1:12, dim = c(3, 4, 1))
  result <- build_tstep(x)
  expect_type(result, "integer")
  expect_length(result, 1)
})

test_that("build_tstep prints a success message via cli", {
  x <- array(1:12, dim = c(3, 4, 1))
  expect_message(build_tstep(x), "Successfully built the number of time steps")
})

test_that("build_tstep errors when check_array rejects non-array input", {
  x <- array(1:12, dim = c(3, 4))
  expect_error(build_tstep(matrix(1:12, nrow = 3))) # adjust if matrices are valid arrays
  expect_error(build_tstep(1:12))
  expect_error(build_tstep(x))
  expect_error(build_tstep(list(1, 2, 3)))
})

# ----- build_ntrans() -----

test_that("build_ntrans computes ntrans correctly for type = 'mean'", {
  result <- build_ntrans(ps_det_example, type = "mean")
  expect_equal(result, 15)
})

test_that("build_ntrans computes ntrans correctly for type = 'min'", {
  result <- build_ntrans(ps_det_example, type = "min")
  expect_equal(result, 18)
})

test_that("build_ntrans computes ntrans correctly for type = 'max'", {
  result <- build_ntrans(ps_det_example, type = "max")
  expect_equal(result, 12)
})

test_that("build_ntrans defaults to type = 'mean' when type is unspecified", {
  expect_equal(
    build_ntrans(ps_det_example),
    build_ntrans(ps_det_example, type = "mean")
  )
})

test_that("build_ntrans returns a single numeric value", {
  result <- build_ntrans(ps_det_example, type = "mean")
  expect_length(result, 1)
  expect_type(result, "double")
})

test_that("build_ntrans errors on invalid type argument", {
  expect_error(build_ntrans(ps_det_example, type = "bogus"))
})

# ----- custom ----
test_that("build_ntrans computes ntrans correctly for type = 'custom'", {
  result <- build_ntrans(ps_det_example, type = "custom", custom_delay = 200)
  expect_equal(result, 18)
})

test_that("build_ntrans errors if type = 'custom' but custom_delay is NULL", {
  expect_error(
    build_ntrans(ps_det_example, type = "custom", custom_delay = NULL)
  )
})

test_that("build_ntrans does not error for non-custom types when custom_delay is NULL", {
  expect_no_error(build_ntrans(
    ps_det_example,
    type = "mean",
    custom_delay = NULL
  ))
})


test_that("build_ntrans errors when min_delay or max_delay is missing, even for type = 'min'", {
  df <- ps_det_example |>
    dplyr::select(-max_delay)
  expect_error(build_ntrans(df, type = "min"))
})

test_that("build_ntrans errors when df is not a data frame", {
  expect_error(build_ntrans(as.list(ps_det_example)))
})

test_that("build_ntrans errors when time_bin column is missing", {
  df <- ps_det_example |> dplyr::select(-time_bin)
  expect_error(build_ntrans(df))
})


test_that("build_ntrans errors when custom_delay is non-numeric", {
  expect_error(build_ntrans(
    ps_det_example,
    type = "mean",
    custom_delay = "fast"
  ))
})

test_that("build_ntrans emits a success message", {
  expect_message(
    build_ntrans(ps_det_example, type = "mean"),
    "Successfully built the number of transmission"
  )
})

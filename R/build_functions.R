# ---- Azimuthal Equidistant projection----

#' Build Azimuthal Equidistant Projection
#'
#' To make computations in Telemetryspace easier
#' Azimuthal Equidistant projection is used which relies
#' on creating a centroid and creating equal distances from that
#' centroid. This function quickly creates the project string
#' needed to supply a `sf` function the crs.
#'
#' @param array_sf the receiver array as an `sf` object.
#'
#' @return a `vector` containing the site specific projection string for
#' the array to be able to supply `sf` functions with a valid
#' crs to transform values into.
#'
#' @name build_functions
#' @export
#'
build_aeqd <- function(array_sf) {
  check_sf_object(array_sf, "array_sf")
  centre <- sf::st_centroid(sf::st_union(array_sf))

  array_crs <- sf::st_crs(array_sf)

  if (array_crs %in% 4326) {
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
  return(aeqd_crs)
}


#' Build Pixel Grid
#'
#' To make a barrier for the model, we need to convert the boundary into
#' pixels that we can use to for the model to recongize where to estimate
#' detection probablity.
#'
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
    sf::st_as_sf(coords = c("x", "y"), crs = st_crs(bnd_sf))

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

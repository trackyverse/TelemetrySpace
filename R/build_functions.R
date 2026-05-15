# ---- builds reflected neightbors ----

#' Build Pixel Grid
#'
#' To make a barrier for the model, we need to convert the boundary into
#' pixels that we can use to for the model to recongize where to estimate
#' detection probablity.
#'
#' @param bnd_sf a `sf` object that is boundary that is desired to impose
#' @param res the resolution desired
#'
#' @return a `list` contain the the number of pixels `n_pixel`, the pixel x coordinates
#' (`pix_x`) and the pixel y coordinates (`pix_y`).
#'
#' @name build_functions
#'
#' @export

build_pixel_grid <- function(bnd_sf, res) {
  check_sf_object(bnd_sf)

  # get boundary box of boundary
  bbox <- sf::st_bbox(bnd_sf)

  # Build centroid grid over bounding box
  gx <- seq(bbox["xmin"] + resolution / 2, bbox["xmax"], by = res)
  gy <- seq(bbox["ymin"] + resolution / 2, bbox["ymax"], by = res)

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

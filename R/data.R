#' Receiver locations from a black sea bass array
#'
#' Projected and scaled receiver coordinates. Scaling is recommended to reduce computation time and prevent convergence issues.
#'
#' @format A data frame with three variables: \code{Station} Receiver ID, \code{east} East-West coordinate,
#'   \code{north} North-South coordinate.
#'
"rlocs"

#' Location of a stationary test transmitter placed in the black sea bass array
#'
#' Projected and scaled as for the receiver coordinates.
#'
#' @format A data frame with two variables: \code{east} East-West coordinate, \code{north} North-South coordinate.
#'
"testloc"

#' Stationary test transmitter data
#'
#' Detection data from a stationary, known-location test tag aggregated to the hour.
#'
#' @format A data frame with five variables: \code{Station} Receiver ID, \code{Transmitter} Transmitter ID,
#'    \code{east} East-West coordinate, \code{north} North-South coordinate, \code{hour} Hour of monitoring.
#'
"testdat"

#' Black sea bass detection data
#'
#' Detection data from a tagged black sea bass aggregated to the hour.
#'
#' @format A data frame with five variables: \code{Station} Receiver ID, \code{Transmitter} Transmitter ID,
#'    \code{east} East-West coordinate, \code{north} North-South coordinate, \code{hour} Hour of monitoring.
#'
"fishdat"

#' Example model parameters
#'
#' Example model parameters for `COA_standard()`
#'
#' @format A data frame with four variables and one row:
#' `nind` which is the number of individuals, `nrec` is the number of receivers
#' in the array, `tsteps` is the number of time steps used in the example,
#' and `ntrans` is the number of expected transmissions within a time step.
#'
"model_param_ex"

#' Example array extent
#'
#' Example array extent used in each model
#'
#' @format A data frame with two variables and two row: `ylim` is the minimum
#' and maximum extent on the y-axis (i.e., latitudinal) for the array and
#' `xlim` is the minimum and maximum extent on the x-axis (i.e., longitudinal)
#' for the array.
#'
"example_extent"

#' Parry Sound `sf` object
#'
#' A `MULTIPOLYGON` `sf` object that is Parry Sound which is a large embayment on
#' Georgian Bay, Lake Huron
#'
#' @format A `sf` object with a single geomety feature. Current coordinate reference system is
#' WGS 84.
#'
"ps"

#' Parry Sound - Receiver Location
#'
#' A `data.frame` that contains the locations of 80 acoustic telemetry receivers deployed
#' in Parry Sound which is a large embayment on Georgian Bay, Lake Huron
#'
#'
#' A `data.frame` with 3 columns and 80 rows.
#'  \describe{
#'    \item{station_no}{The station number of the receiver}
#'    \item{deploy_lat}{The latitude of the deployed receiver}
#'    \item{deploy_long}{The longitude of the deployed receiver}
#' }
#'
"ps_rec_loc"

#' Detection `data.frame` for a tagged Lake Trout
#'
#'
#' A `data.frame` that contains detections data for a tagged Lake Trout in
#' Parry Sound which is a large embayment on Georgian Bay, Lake Huron
#'
#'
#' @format A `data.frame` with 5 columns and 577 rows.
#'  \describe{
#'    \item{detection_timestamp_utc}{The detection timestamp as `POSIXct` with a tz of UTC}
#'    \item{station_no}{The receiver station number}
#'    \item{tag_serial_no}{The tag serial number}
#'    \item{min_delay}{The minimum delay between transmissions in seconds}
#'    \item{max_delay}{The maximum delay between transmissions in seconds}
#' }
#'
"ps_det_example"
#' Detection `data.frame` for a test tag
#'
#'
#' A `data.frame` that contains detections data for aan internal transmitter
#' of a VR2AR deployed in Parry Sound which is a large embayment on Georgian Bay, Lake Huron
#'
#'
#' @format A `data.frame` with 7 columns and 200 rows.
#'  \describe{
#'    \item{detection_timestamp_utc}{The detection timestamp as `POSIXct` with a tz of UTC}
#'    \item{station_no}{The receiver station number the tag was detected on}
#'    \item{tag_serial_no}{The serial number of the VR2AR receiver}
#'    \item{tag_id}{The tag id value from of the VR2AR receiver}
#'    \item{tag_station_no}{The station number of the tag}
#'    \item{min_delay}{The minimum delay between transmissions in seconds}
#'    \item{max_delay}{The maximum delay between transmissions in seconds}
#' }
#'
"ps_det_test_tag"

#' Parry Sound - Test Tag Locations object
#'
#' A `data.frame` that contains the `tag_station_no` and the deployment latitude and longitude of
#' the test tag deployed in Parry Sound which is a large embayment on Georgian Bay, Lake Huron
#'
#'
#' A `data.frame` with 3 columns and 1 rows.
#'  \describe{
#'    \item{station_no}{The station number of the tag}
#'    \item{deploy_lat}{The latitude of the deployed tag}
#'    \item{deploy_long}{The longitude of the deployed tag}
#' }

"ps_test_tag_loc"


#' Counts of detection per time steps
#'
#' Array of counts of detection per time step per receiver. Originally with
#' dimensions of 1 (individual) x 30 (receivers) x 10 (time steps), changed to
#' 1 x 10 x 30 on 2026-05-13 for computational efficiency.
#'
#' @format An array with dimensions of 1 by 10 (number of time steps) by 30 (number
#' of receivers).
#'
"Y"

#' Counts of detection per time steps for test tag
#'
#' Array of counts of detection per time step per receiver for the test tag.
#' Originally with dimensions of 1 (individual) x 30 (receivers) x 10 (time steps),
#' changed to 1 x 10 x 30 on 2026-05-13 for computational efficiency.
#'
#' @format An array with dimensions of 1 by 10 (number of time steps) by 30 (number
#' of receivers).
#'
"testY"

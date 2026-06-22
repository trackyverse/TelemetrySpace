# Build Functions

These functions structure and build data components that are needed by
the model. Each function will either return an object that is properly
built and structured for \`Stan“ or will produce an object to be latter
used in the pre-process phase of analysis.

## Usage

``` r
build_aeqd(array_sf)

build_bbox(coord_df, buffer = NULL)

build_counts(df, nrec, rec_id, rec_names = NULL)

build_init(coord_df, nind, tstep)

build_ntrans(df, type = c("mean", "min", "max", "custom"), custom_delay = NULL)

build_pixel_grid(bnd_sf, res, crs)

build_rec_coords(obj_sf)

build_time_bin(df, unit = "1 hour")

build_tstep(x)
```

## Arguments

- array_sf:

  the receiver array as an `sf` object.

- coord_df:

  a `data.frame` that contains two columns named `recX` and `recY`
  created by `build_rec_coords()`.

- buffer:

  a `numerical` value to set the buffer. Defaults to `1`. Considering
  the default Azimuthal Equidistant projection is km, 1 represents a 1
  km buffer.

- df:

  a `data.frame` that contains the following column names
  `tag_serial_no`

- nrec:

  a `numerical` value that is number of receivers in the telemetry
  array.

- rec_id:

  a `vector` that matches the length of `nrec` and needs to be index of
  the receivers in the telemetry array.

- rec_names:

  an optional `vector` that contains the station names of the receivers.
  If not supplied it will default to using `rec_id`

- nind:

  a `numerical` value to the number of individuals.

- tstep:

  a `numerical` value to the number of time steps.

- type:

  can either be `mean`, `min`, `max`, or `custom`. This will change how
  the number of transmissions that are expected within a time bin are
  calculated. When set to `mean` (the deafult), the function will
  calculate the mean time delay (s) between the minimumm and maximum
  delay (s), while `min` and `max`, will use the minimumm or maximum
  delay value, respectively. If specifying `custom` (see
  `custom_delay`), the user can enter the delay of their choosing.
  Adjusting the delays used can useful becaue the number of detections
  within a time bin can exceed the number of transmissions expected
  wtihin the time bin which will cause the model to fail.

- custom_delay:

  Only needed when `type` is set to `"custom"`. When supplied, this
  argument, which is a `numeric` will allow a custom value to be used
  for the tag delays.

- bnd_sf:

  a `sf` object that is boundary that is desired to impose

- res:

  the resolution desired.

- crs:

  the Azimuthal Equidistant projection desired. If the supplied
  projection string does not contain `"+proj=aeqd"`. The function will
  error.

- obj_sf:

  a `sf` object that the receiver locations as `POINT` geometry. The
  `sf` object has to be in Azimuthal Equidistant projection.

- unit:

  a `character` that is the unit desired to bin. Default is `"1 hour"`.
  See
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)'s
  `unit` argument for more details.

- x:

  a 3-dimensional count array.

## Value

`build_aeqd()`- retruns a `vector` containing the site specific
projection string for the array to be able to transform the crs.

`build_bbox()`- returns a `data.frame` containing two columns named
`xlim` and `ylim` which are the minimum and maximum values +/- a buffer
for x and y.

`build_counts()` - returns a 3-dimensional `array` containing the number
of detections for the following dimensions, the number of invividuals,
by the number of time bins, by the number of receivers.

`build_init()`- returns a `list` containing two matrices named `sx` and
`sy`. Each matrix is the mean of either `recX` or `recY` produced by
`build_rec_coords()` by the number of individuals by the number of
timesteps.

`build_ntrans()` - retruns a single value vector.

`build_pixel_grid()` - returns a `list` contain the the number of pixels
`n_pixel`, the pixel x coordinates (`pix_x`) and the pixel y coordinates
(`pix_y`).

`build_rec_coords()` - a `data.frame` containing two `columns` named
`recX` and `recY` which are the receiver locations transformed into
Azimuthal Equidistant projection.

`build_time_bin()`- returns a `data.frame` that has had the columns
`time_bin` and `time`added. `time` is an index value of `time_bin` and
is needed by the model. This will further be used by `build_counts()`.

`build_tstep()` - a numerical value that is the number of timesteps.

## Details

`build_aeqd()`- Azimuthal Equidistant projection is needed by the model
which relies on creating a centroid and creating equal distances from
the centroid. This function quickly creates the project string needed to
transform an exisiting crs to Azimuthal Equidistant projection.

`build_bbox()`- builds the boundary box of receiver array needed by the
model.

`build_counts()` - builds a 3-dimensional count `array`. The models need
the counts at each recevier for each time bin for each individual
structure in 3-dimensional `array`. This functions takes in a detection
`data.frame`, creates a count `data.frame` and then transforms it into a
3-dimensional `array` that will be pased to the `Stan` model.

`build_init()`- builds the initial values to supply to the model.
Supplying these values can helps the model know where to intially start.

`build_ntrans()` - builds the nubmer of transmissions to be expected
within a given time bin.

`build_pixel_grid()` - builds barrier grid for the model. We need to
convert the boundary into pixels that can be used to recongize where to
estimate detection probablity. The boundary that is supplied needs to be
in UTMs, with the returned values being in Azimuthal Equidistant
projection. To build this projection see `build_aeqd()`. When supplying
the desired resolution remember that this is in m so a value of `1`
would be quite small while of `1000` is 1 km which makes a less dense
grid.

`build_rec_coords()` - builds receiver coordinates as the models need
the easting and northing (i.e., x and y) coordinates of the receivers.

`build_time_bin()`- builds and adds time bins to the detection
`data.frame`.

`build_tstep()` - builds the number of total time steps that exist
whithin the supplied 3-dimensional count array.

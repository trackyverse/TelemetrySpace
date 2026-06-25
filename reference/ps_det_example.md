# Detection `data.frame` for a tagged Lake Trout

A `data.frame` that contains detections data for a tagged Lake Trout in
Parry Sound which is a large embayment on Georgian Bay, Lake Huron

## Usage

``` r
ps_det_example
```

## Format

A `data.frame` with 5 columns and 577 rows.

- detection_timestamp_utc:

  The detection timestamp as `POSIXct` with a tz of UTC

- station_no:

  The receiver station number

- tag_serial_no:

  The tag serial number

- min_delay:

  The minimum delay between transmissions in seconds

- max_delay:

  The maximum delay between transmissions in seconds

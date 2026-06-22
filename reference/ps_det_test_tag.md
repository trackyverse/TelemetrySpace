# Detection `data.frame` for a test tag

A `data.frame` that contains detections data for aan internal
transmitter of a VR2AR deployed in Parry Sound which is a large
embayment on Georgian Bay, Lake Huron

## Usage

``` r
ps_det_test_tag
```

## Format

A `data.frame` with 7 columns and 214 rows.

- detection_timestamp_utc:

  The detection timestamp as `POSIXct` with a tz of UTC

- station_no:

  The receiver station number the tag was detected on

- tag_serial_no:

  The serial number of the VR2AR receiver

- tag_id:

  The tag id value from of the VR2AR receiver

- tag_station_no:

  The station number of the tag

- min_delay:

  The minimum delay between transmissions in seconds

- max_delay:

  The maximum delay between transmissions in seconds

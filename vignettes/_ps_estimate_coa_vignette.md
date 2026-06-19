---
title: "Estimate Centers of Activity for Lake Trout Tagged with an Acoustic Transmitter"
author: Benjamin L. Hlina
date: "2026-06-13"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Estimate Centers of Activity for Lake Trout Tagged with an Acoustic Transmitter}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


## Introduction

This vignette will walk you through the analyses presented in Winton et al. 2018, who describe the use of spatial point process models to estimate individual centers of activity from passive acoustic telemetry data. This vignette progresses walks through how to prepare the data, using the model, and interpretating the results. We will be using the simplest case, which assumes that detection probabilities/receiver detection ranges remain constant over time, to application of a test-tag integrated model, that incorporates detection data from one or more stationary test transmitters to estimate time-varying detection ranges. The models are fitted in a Bayesian framework using the Stan software (Carpenter et al. 2017); code was modified from that provided in Royle et al. 2013 for fitting spatial point process models to data from camera traps. We prefer the Bayesian approach for COA estimation due to its treatment of uncertainty, but realize the longer computational time required may be prohibitive for some applications. We'd also like to note that the models described can support varying degrees of complexity - not all applications will require (or have the data to support) the most complex version of the model. The simpler the model, the shorter the run-time.

We have tried to make the instructions outlined in this vignette user-friendly since we are a group of applied biologists with varying degrees of statistical experience. If some of the statistical notation outlined here or in the paper remains unclear, feel free to contact us with questions for clarification. This is a new package, so if you find bugs, places where code efficiency could be improved, or instances where the documentation could be made more user-friendly, please let us know! 
 
## Data preparation

To run spatial point process/detection models in Stan we need to do a couple of things to our detection and receiver location data to first provide the model with all the necessary information as well as in the proper format. Stan likes to handle data in vectors and 
arrays, which we don't often use in R, that are then provided to stan in a `list`. To learn more about Stan, please click this [link](https://mc-stan.org/). Stan is written in C++, making it quite fast, with Stan programs following a very systematic order. Stan uses [Markov chain Monte Carlo (MCMC)](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) and [No U-Turn Sampler (NUTS)](https://mc-stan.org/docs/2_18/stan-users-guide/sampling-difficulties-with-problematic-priors.html) to improve efficiency. We will now walk through how to setup the data to be able to run the models. 

We have serveral example datasets along with several functions to assist and streamline the data preparation. First we need to evaluate the positions of the receivers and create a [Azimuthal Equal Distance projection (aeqd)](https://en.wikipedia.org/wiki/Azimuthal_equidistant_projection). We use an aeqd set in kilometers (km) for several reasons, the first being that in Stan it is very efficent to calcuate distances among receivers, which one thing the model does, when the distance ranges between 0.2 - 15 km apart. The second is because....(Mike I need you to add stuff here as you know more about aeqd). We will be working with example data for a single Lake Trout (*Salvlinus namaycush*) that was implanted with an acoustic transmitter in Parry Sound which is a large embayment of Geogian Bay, Lake Huron. 

First we load all the packages needed to carry out the analysis.


``` r
{
  library(TelemetrySpace)
  library(sf)
}
```

### Receiver Locations 

Next we will look at the location of the receivers in Parry Sound. To build a aqed projection we need the locations to 
be in metre which we can transform these locations to a UTM crs that will be in metres. You will notice that `ps_rec_loc` is a 
`sf` object. If your receiver locations are not already an `sf` object you will need to make them into one which can be done using 
`st_as_sf()` from [{sf}](https://r-spatial.github.io/sf/). 

``` r
# first look at the example
head(ps_rec_loc)
#> # A tibble: 6 × 3
#>   station_no deploy_lat deploy_long
#>   <chr>           <dbl>       <dbl>
#> 1 PSM-001          45.3       -80.1
#> 2 PSM-002          45.3       -80.1
#> 3 PSM-003          45.3       -80.1
#> 4 PSM-004          45.3       -80.2
#> 5 PSM-005          45.3       -80.2
#> 6 PSM-006          45.3       -80.2
str(ps_rec_loc)
#> tibble [80 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ station_no : chr [1:80] "PSM-001" "PSM-002" "PSM-003" "PSM-004" ...
#>  $ deploy_lat : num [1:80] 45.3 45.3 45.3 45.3 45.3 ...
#>  $ deploy_long: num [1:80] -80.1 -80.1 -80.1 -80.2 -80.2 ...

# make it into a sf object

ps_rec_loc_sf <- ps_rec_loc |>
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)
# next transform it into the correct utm

ps_rec_loc_utm <- ps_rec_loc_sf |>
  st_transform(32617)
```

Next we will build an aeqd project for this array that we can then transform the locations of the receivers into. 

``` r
aeqd_crs <- build_aeqd(ps_rec_loc_utm)
#> ✔ Successfully built "+proj=aeqd +lon_0=-80.124804 +lat_0=45.333008 +x_0=0 +y_0=0 +datum=WGS84 +units=km"
```

We then can transform the receiver locations into our aeqd project

``` r
ps_rec_loc_aeqd <- ps_rec_loc_sf |>
  st_transform(aeqd_crs) |>
  (\(.) .[order(.$station_no), ])()
```
Now that we the receiver locations transformed we need to first index them appropiately, this is because Stan will not be able to handel 
the `station_no` but instead can handle a numerical index value for the receivers. 

``` r
ps_rec_loc_aeqd$rec <- 1:nrow(ps_rec_loc_aeqd)
```

Next we will transform this into two vectors that Stan can handel - the function will return a list with these two vectors as well 
as create the boundary box/limits. The buffer the limit assumes is 1 km - this can be changed depending on how you would like the boundary
box/limits extend. 

``` r
rec_loc_vec <- build_rec_coords(ps_rec_loc_aeqd)

rec_limits <- build_bbox(rec_loc_vec)
```

### Detection Data 

Now that we have the receiver locations in a format that Stan can handle, we are going to prepare the detection data. First lets look at the detection data. 

``` r
head(ps_det_example)
#> # A tibble: 6 × 5
#>   detection_timestamp_utc station_no tag_serial_no min_delay max_delay
#>   <dttm>                  <chr>      <chr>             <dbl>     <dbl>
#> 1 2024-05-03 20:32:49     PSM-007    1594061             190       290
#> 2 2024-05-03 20:41:12     PSM-005    1594061             190       290
#> 3 2024-05-03 20:41:12     PSM-007    1594061             190       290
#> 4 2024-05-03 20:41:13     PSM-019    1594061             190       290
#> 5 2024-05-03 20:45:18     PSM-007    1594061             190       290
#> 6 2024-05-03 20:48:34     PSM-007    1594061             190       290
str(ps_det_example)
#> tibble [592 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ detection_timestamp_utc: POSIXct[1:592], format: "2024-05-03 20:32:49" "2024-05-03 20:41:12" "2024-05-03 20:41:12" "2024-05-03 20:41:13" ...
#>  $ station_no             : chr [1:592] "PSM-007" "PSM-005" "PSM-007" "PSM-019" ...
#>  $ tag_serial_no          : chr [1:592] "1594061" "1594061" "1594061" "1594061" ...
#>  $ min_delay              : num [1:592] 190 190 190 190 190 190 190 190 190 190 ...
#>  $ max_delay              : num [1:592] 290 290 290 290 290 290 290 290 290 290 ...
```

We can notice that this detection data consists of 8 columns with 592 rows - to better understand which each field is you can run `?ps_det_example` to review the full documentation.

For our detection data, we have a few things we need to do, the first is we need to build a time bin that we will evaluate the data over. For this data we are going to use 1 hour but this time bin can range from 30 mins - 1 day or more and depends on the questions you are asking and the species you are working with. 

Lets build our time bins


``` r
ps_det_example <- build_time_bin(ps_det_example, unit = "1 hour")
head(ps_det_example)
#> # A tibble: 6 × 7
#>   detection_timestamp_utc station_no tag_serial_no min_delay max_delay time_bin             time
#>   <dttm>                  <chr>      <chr>             <dbl>     <dbl> <dttm>              <int>
#> 1 2024-05-03 20:32:49     PSM-007    1594061             190       290 2024-05-03 20:00:00     1
#> 2 2024-05-03 20:41:12     PSM-005    1594061             190       290 2024-05-03 20:00:00     1
#> 3 2024-05-03 20:41:12     PSM-007    1594061             190       290 2024-05-03 20:00:00     1
#> 4 2024-05-03 20:41:13     PSM-019    1594061             190       290 2024-05-03 20:00:00     1
#> 5 2024-05-03 20:45:18     PSM-007    1594061             190       290 2024-05-03 20:00:00     1
#> 6 2024-05-03 20:48:34     PSM-007    1594061             190       290 2024-05-03 20:00:00     1
```

You will notice both a `POSIXct` column that is called `time_bin` and a numerical column called `time`. This `time` column is a numerical index of the time bins. 

We have a few more things we need to do to prepare the data, first we need to add in the numerical index of the receiver values that we created in the first section. We can do this by using `merge()` from base or we want we could use `left_join()` from `{dplyr}`. 

``` r
ps_det_example <- merge(
  ps_det_example,
  st_drop_geometry(ps_rec_loc_aeqd),
  by = "station_no"
)
head(ps_det_example)
#>   station_no detection_timestamp_utc tag_serial_no min_delay max_delay            time_bin time rec
#> 1    PSM-001     2024-05-04 02:09:48       1594061       190       290 2024-05-04 02:00:00    7   1
#> 2    PSM-001     2024-05-04 04:05:03       1594061       190       290 2024-05-04 04:00:00    9   1
#> 3    PSM-001     2024-05-04 03:37:50       1594061       190       290 2024-05-04 03:00:00    8   1
#> 4    PSM-002     2024-05-04 03:11:04       1594061       190       290 2024-05-04 03:00:00    8   2
#> 5    PSM-002     2024-05-04 02:40:09       1594061       190       290 2024-05-04 02:00:00    7   2
#> 6    PSM-002     2024-05-03 22:57:37       1594061       190       290 2024-05-03 22:00:00    3   2
```
We are starting to get somewhere, you can see that we now have the time and recevier index values lined up. The last two things we need to do is first determine the numer of individuals - in this case it will be 1 and for right now we likely suggest running the model for each invidual with extended time periods being broken into chunks for example 1 years worth of data being broken down into 7 day chunks that are then model, the detection example transformed into the number of detections for invidiaul for each receiver for each time bin, and the number of transmissions this is where `min_delay` and `max_delay` come into play. 

Lets build the count data and the number of time steps in the data. 


``` r
ps_count_example <- build_counts(
  df = ps_det_example,
  nrec = nrow(ps_rec_loc_aeqd),
  rec_id = ps_rec_loc_aeqd$rec,
  rec_names = ps_rec_loc_aeqd$station_no
)

time_steps <- build_tstep(ps_count_example)
#> ✔ Successfully built the number of time steps 9
```

Now we can create the number of inviduals and the number of transmisisons. 

``` r
nind <- length(unique(ps_det_example$tag_serial_no))

ntrans <- build_ntrans(ps_det_example)
#> ✔ Successfully built the number of transmission 15 expectd in "1 hour(s)" bins based off of
#> "mean delay".
```

We can finally move on to running the model 

## Model 
We can now run the standard point process/detection probablity model 

``` r
m <- COA_Standard(
  nind = nind,
  nrec = nrow(ps_rec_loc_aeqd),
  ntime = time_steps,
  ntrans = ntrans,
  y = ps_count_example,
  recX = rec_loc_vec$recX,
  recY = rec_loc_vec$recY,
  xlim = rec_limits$xlim,
  ylim = rec_limits$ylim,
  chains = 2,
  thin = 5
)
#> 
#> SAMPLING FOR MODEL 'COA_Standard_gaussian' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.001118 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 11.18 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 23.775 seconds (Warm-up)
#> Chain 1:                17.576 seconds (Sampling)
#> Chain 1:                41.351 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'COA_Standard_gaussian' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.001114 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 11.14 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 17.813 seconds (Warm-up)
#> Chain 2:                17.303 seconds (Sampling)
#> Chain 2:                35.116 seconds (Total)
#> Chain 2: 
#>         warmup sample
#> chain:1 23.775 17.576
#> chain:2 17.813 17.303
```

# Estimate Centers of Activity for Lake Trout Tagged with an Acoustic Transmitter

## Introduction

This vignette will walk you through the analyses presented in [Winton et
al. 2018](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13080),
who describes the use of spatial point process models to estimate
individual centers of activity (COA) from passive acoustic telemetry
data. This vignette walks through how to prepare the data, using the
models, and interpretating the results. We will be using the simplest
case, which assumes that detection probabilities/receiver detection
ranges remain constant over time, to a more complex application of a
test-tag integrated model, that incorporates detection data from one or
more stationary test transmitters to estimate time-varying detection
ranges. The models are fitted in a Bayesian framework using the Stan
software ([Carpenter et
al. 2017](https://www.jstatsoft.org/article/view/v076i01/0)); code was
modified from models found in [Royle et
al. 2013](https://www.sciencedirect.com/book/monograph/9780124059399/spatial-capture-recapture)
for fitting spatial point process models to data from camera traps. We
prefer the Bayesian approach for COA estimation due to the treatment of
uncertainty, but realize the longer computational time required may be
prohibitive for some applications. We’d also like to note that the
models described can support varying degrees of complexity - not all
applications will require (or have the data to support) the most complex
version of the model. The simpler the model, the shorter the run-time.

We have tried to make the instructions outlined in this vignette
user-friendly since we are a group of applied biologists with varying
degrees of statistical experience. If some of the statistical notation
outlined here or in [Winton et
al. 2018](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13080)
remains unclear, feel free to contact us with questions for
clarification. This is a new package, so if you find bugs, places where
code efficiency could be improved, or instances where the documentation
could be made more user-friendly, please let us know through the
[issues](https://github.com/trackyverse/TelemetrySpace) on the GitHub
repository!

## Models

For simplicity purposes of this vignette, model specifications can be
found in the following documentation:

- [Standard detection
  probability](https://telemetryspace.trackyverse.org/articles/coa_standard_gaussian_model.html)
  (i.e., `COA_standard()`)
- [Time-varying detection
  probability](https://trackyverse.github.io/TelemetrySpace/articles/)
  (i.e.,
  [`COA_TimeVarying ()`](https://trackyverse.github.io/TelemetrySpace/reference/COA_TimeVarying.md))
- [Tag-integrated time-varying detection
  probability](https://trackyverse.github.io/TelemetrySpace/articles/)
  (i.e.,
  [`COA_TagInt()`](https://trackyverse.github.io/TelemetrySpace/reference/COA_TagInt.md))

## Data preparation

To run spatial point process/detection probability models in Stan we
need to do a couple of things to our detection and receiver location
data. This includes first providing the model with all the necessary
information as well as in the proper format. Stan likes to handle data
using vectors and arrays, which we often use in R but they are
configured differently (e.g.,
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) is a bunch
vectors of different classes, while a `matrix` is a bunch of vectors of
the same class). These vectors and arrays need to be provided to Stan in
a `list`. To learn more about Stan, please click this
[link](https://mc-stan.org/). Stan is written in C++, making it quite
fast, with Stan programs following a very systematic order, see [link to
understand a Stan
program](https://mc-stan.org/docs/reference-manual/blocks.html). Stan
uses a [Hamilton Monte
Carlo](https://mc-stan.org/docs/2_18/reference-manual/hamiltonian-monte-carlo.html)
which is a [Markov chain Monte Carlo
(MCMC)](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) that
obtains a sequence of random samples whose distribution converges to a
target probability distribution that is difficult to sample directly.
Stan also employs a [No U-Turn Sampler
(NUTS)](https://mc-stan.org/docs/2_18/stan-users-guide/sampling-difficulties-with-problematic-priors.html)
to improve efficiency. We will now walk-through how-to setup the data to
be able to run the models.

We have several example datasets along with several functions to assist
and streamline the data preparation. First, we need to evaluate the
positions of the receivers and create a [Azimuthal Equal Distance
projection
(aeqd)](https://en.wikipedia.org/wiki/Azimuthal_equidistant_projection).
We use an aeqd project set in kilometers (km) for several reasons, the
first being that in Stan it is very efficient to calculate distances
among receivers, however, the distance between receivers needs to be on
the same x and y plane and ranges need to be between 0.2 - 20 km apart.
The second is because….(Mike I need you to add stuff here as you know
more about aeqd). We will be working with example data for a single Lake
Trout (*Salvelinus namaycush*) that was implanted with an acoustic
transmitter in Parry Sound which is a large embayment of Georgian Bay,
Lake Huron.

First, we load all the packages needed to carry out the analysis.

``` r

{
  library(bayesplot)
  library(ggplot2)
  library(rstan)
  library(sf)
  library(TelemetrySpace)
  library(tidyr)
}
```

### Receiver Locations

Next, we will look at the location of the receivers in Parry Sound. To
build an aqed projection we need the locations to be in meters which we
can transform these locations to a meter focused coordinate reference
system (crs), in this case a projected coordinate system (e.g., UTMs).
You will notice that `ps_rec_loc` is a `data.frame`. We need to make it
into an `sf` object which is in a geographical coordinate reference
system (e.g., WGS 84; degrees longitude and latitude). We need to
transform it into a projected coordinate system, in this case we used
UTMs. We then can build the aeqd projection. We can do this using
[`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
from [{sf}](https://r-spatial.github.io/sf/).

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
# next transform it into projected coordinate system (e.g., utm ESPG:32617)

ps_rec_loc_utm <- ps_rec_loc_sf |>
  st_transform(32617)
```

Next, we will build an aeqd projection for this receiver array that we
can then transform the locations of the receivers into.

``` r

aeqd_crs <- build_aeqd(ps_rec_loc_utm)
#> ✔ Successfully built "+proj=aeqd +lon_0=-80.124804 +lat_0=45.333008 +x_0=0 +y_0=0 +datum=WGS84 +units=km"
```

We then can transform the receiver locations into our aeqd project and
make sure they are in the correct order.

``` r

ps_rec_loc_aeqd <- ps_rec_loc_sf |>
  st_transform(aeqd_crs) |>
  (\(.) .[order(.$station_no), ])()
```

Now that we have the receiver locations transformed, we need to first
index them appropriately, this is because Stan will not be able to
handle the `station_no` but instead can handle a numerical index value
to identify the receivers.

``` r

ps_rec_loc_aeqd$rec <- 1:nrow(ps_rec_loc_aeqd)
```

Next, we will transform this into a `data.frame` the contains two
columns (i.e.,`vectors`) that we can supply to the Stan model. We also
need to build the boundary box of the receiver array. The argument
`buffer` in
[`build_bbox()`](https://trackyverse.github.io/TelemetrySpace/reference/build_functions.md)
assumes a 1 km buffer but this can be changed depending on how you would
like the boundary box to be created.

``` r

rec_loc_vec <- build_rec_coords(ps_rec_loc_aeqd)

rec_limits <- build_bbox(rec_loc_vec)
```

### Detection Data

Now that we have the receiver locations in a format that Stan can
handle, we are going to prepare the detection data. First let’s look at
the detection data.

``` r

head(ps_det_example)
#>   station_no detection_timestamp_utc tag_serial_no min_delay max_delay            time_bin time rec.x rec.y
#> 1    PSM-001     2024-05-04 02:09:48       1594061       190       290 2024-05-04 02:00:00    6     1     1
#> 2    PSM-001     2024-05-04 04:05:03       1594061       190       290 2024-05-04 04:00:00    8     1     1
#> 3    PSM-001     2024-05-04 03:37:50       1594061       190       290 2024-05-04 03:00:00    7     1     1
#> 4    PSM-002     2024-05-04 03:11:04       1594061       190       290 2024-05-04 03:00:00    7     2     2
#> 5    PSM-002     2024-05-04 02:40:09       1594061       190       290 2024-05-04 02:00:00    6     2     2
#> 6    PSM-002     2024-05-03 22:57:37       1594061       190       290 2024-05-03 22:00:00    2     2     2
str(ps_det_example)
#> 'data.frame':    577 obs. of  9 variables:
#>  $ station_no             : chr  "PSM-001" "PSM-001" "PSM-001" "PSM-002" ...
#>  $ detection_timestamp_utc: POSIXct, format: "2024-05-04 02:09:48" "2024-05-04 04:05:03" "2024-05-04 03:37:50" "2024-05-04 03:11:04" ...
#>  $ tag_serial_no          : chr  "1594061" "1594061" "1594061" "1594061" ...
#>  $ min_delay              : num  190 190 190 190 190 190 190 190 190 190 ...
#>  $ max_delay              : num  290 290 290 290 290 290 290 290 290 290 ...
#>  $ time_bin               : POSIXct, format: "2024-05-04 02:00:00" "2024-05-04 04:00:00" "2024-05-04 03:00:00" "2024-05-04 03:00:00" ...
#>  $ time                   : int  6 8 7 7 6 2 6 3 3 8 ...
#>  $ rec.x                  : int  1 1 1 2 2 2 2 2 2 2 ...
#>  $ rec.y                  : int  1 1 1 2 2 2 2 2 2 2 ...
```

We can notice that this detection data consists of 5 columns with 577
rows. To better understand what each field is you can run
[`?ps_det_example`](https://trackyverse.github.io/TelemetrySpace/reference/ps_det_example.md)
to review the full documentation.

For our detection data, we have a few things we need to do, the first is
we need to build a time bin that we will create COAs. For this data we
are going to use 1 hour but this time bin can range from 30 mins - 1 day
or more and depends on the questions you are asking and the species you
are working with.

Let’s build our time bins

``` r

ps_det_example <- build_time_bin(ps_det_example, unit = "1 hour")
head(ps_det_example)
#>   station_no detection_timestamp_utc tag_serial_no min_delay max_delay            time_bin time rec.x rec.y
#> 1    PSM-007     2024-05-03 21:01:53       1594061       190       290 2024-05-03 21:00:00    1     7     7
#> 2    PSM-003     2024-05-03 21:01:54       1594061       190       290 2024-05-03 21:00:00    1     3     3
#> 3    PSM-004     2024-05-03 21:05:25       1594061       190       290 2024-05-03 21:00:00    1     4     4
#> 4    PSM-007     2024-05-03 21:05:25       1594061       190       290 2024-05-03 21:00:00    1     7     7
#> 5    PSM-008     2024-05-03 21:05:25       1594061       190       290 2024-05-03 21:00:00    1     8     8
#> 6    PSM-003     2024-05-03 21:05:26       1594061       190       290 2024-05-03 21:00:00    1     3     3
```

You will notice both a `POSIXct` column that is called `time_bin` and a
numerical column called `time`. This `time` column is a numerical index
of the time bins.

We have a few more things we need to do to prepare the data, first we
need to add in the numerical index of the receiver values that we
created in the first section. We can do this by using
[`merge()`](https://rdrr.io/r/base/merge.html) from base or we could use
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
from [dplyr](https://dplyr.tidyverse.org) or `merge.data.table()` from
[data.table](https://r-datatable.com).

``` r

ps_det_example <- merge(
  ps_det_example,
  st_drop_geometry(ps_rec_loc_aeqd),
  by = "station_no"
)
head(ps_det_example)
#>   station_no detection_timestamp_utc tag_serial_no min_delay max_delay            time_bin time rec.x rec.y rec
#> 1    PSM-001     2024-05-04 02:09:48       1594061       190       290 2024-05-04 02:00:00    6     1     1   1
#> 2    PSM-001     2024-05-04 04:05:03       1594061       190       290 2024-05-04 04:00:00    8     1     1   1
#> 3    PSM-001     2024-05-04 03:37:50       1594061       190       290 2024-05-04 03:00:00    7     1     1   1
#> 4    PSM-002     2024-05-04 03:11:04       1594061       190       290 2024-05-04 03:00:00    7     2     2   2
#> 5    PSM-002     2024-05-04 02:40:09       1594061       190       290 2024-05-04 02:00:00    6     2     2   2
#> 6    PSM-002     2024-05-03 22:57:37       1594061       190       290 2024-05-03 22:00:00    2     2     2   2
```

We are starting to get somewhere; you can see that we now have the time
and receiver index values lined up. The last two things we need to do is
first determine the number of individuals, in this case it will be 1 and
the expected number of detections for an individual for each receiver
for each time bin, and the number of transmissions this is where
`min_delay` and `max_delay` come into play.

Let’s build the count data and the number of time steps in the data.

``` r

ps_count_example <- build_counts(
  df = ps_det_example,
  nrec = nrow(ps_rec_loc_aeqd),
  rec_id = ps_rec_loc_aeqd$rec,
  rec_names = ps_rec_loc_aeqd$station_no
)

time_steps <- build_tstep(ps_count_example)
#> ✔ Successfully built the number of time steps 8
```

Now we can create the number of individuals and the number of
transmissions expected within the time bin.

``` r

nind <- length(unique(ps_det_example$tag_serial_no))

ntrans <- build_ntrans(ps_det_example)
#> ✔ Successfully built the number of transmission 15 expectd in "1 hour(s)" bins based off of
#> "mean delay".
```

We can finally move on to running the model; however, we need to
understand a bit more about Bayesian frameworks before we proceed.

## Bayesian Analysis

We can now estimate the center of activity for lake trout in Parry Sound
which is a large embayment of Georgian Bay, Lake Huron.

There are a few things to know about running a Bayesian analysis, we
suggest reading these resources:

1.  [Basics of Bayesian Statistics -
    Book](https://statswithr.github.io/book/)
2.  [A Student’s Guide to Bayesian Statistics by Ben
    Lambert](https://sites.math.rutgers.edu/~zeilberg/EM20/Lambert.pdf)
3.  [Statistical rethinking 2 with rstan and the tidyverse by Solomon
    Kurz](https://solomon.quarto.pub/sr2rstan/)
4.  [Fitting Bayesian Models using Stan and R by Dan
    Ovando](https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/)
5.  [Andrew Proctor’s - Module
    6](https://andrewproctor.github.io/rcourse/module6.html)
6.  [van de Schoot et al.,
    (2021)](https://www.nature.com/articles/s43586-020-00001-2)

### Priors

Bayesian analyses rely on supplying uninformed or informed prior
distributions for each parameter (coefficient; predictor) in the model.
For the puproses of the deteciton probablity model the following priors
are assumed and are not adjustable. To learn more about the structure of
the priors and likelihoods see [Standard detection
probability](https://telemetryspace.trackyverse.org/articles/coa_standard_gaussian_model.html).

``` math
\begin{aligned}
\alpha_0 &\sim \mathrm{Cauchy}(0,\, 2.5), &&\text{truncated to } [-5, 5] \\
\alpha_1 &\sim \mathrm{Cauchy}(0,\, 2.5), &&\text{truncated to } (0, \infty) \\
s_{x,i,t} &\sim \mathrm{Uniform}(x_{\min},\, x_{\max}) &&\text{for all } i, t \\
s_{y,i,t} &\sim \mathrm{Uniform}(y_{\min},\, y_{\max}) &&\text{for all } i, t
\end{aligned}
```

The Cauchy priors on $`\alpha_0`$ and $`\alpha_1`$ are weakly
informative and regularize the intercept and decay-rate parameters
toward zero while allowing heavy tails. Because both parameters carry
explicit `lower`/`upper` bounds in the `parameters` block, the priors
are implicitly truncated to those bounds — Stan automatically
renormalizes the density over the constrained support, so no separate
normalizing constant needs to be added by hand. The activity-center
coordinates $`s_{x,i,t}`$ and $`s_{y,i,t}`$ have no explicit sampling
statement, so they receive Stan’s implicit flat (uniform) prior over
their bounded support $`[x_{\min}, x_{\max}]`$ and
$`[y_{\min}, y_{\max}]`$, respectively.

### Model convergence

It is important to always run the model with at least 2 chains. If the
model does not converge you can try to increase or decreasing the
following:

1.  The number of samples that are discarded; this can be controlled by
    the argument `warmup`.

2.  The number of iterative samples retained; this can be controlled by
    the argument `iter`.

3.  The number of samples drawn (in this is controlled by the argument
    `thin`). This will also reduce the size the posterior distribution
    kept.

4.  The `adapt_delta` value using `control = list(adapt_delta = 0.95)`.

When assessing the model, we want $`\hat R`$ to be 1 or within 0.05 of
1, which indicates that the variance among and within chains are equal
(see [{rstan} documentation on
$`\hat R`$](https://mc-stan.org/rstan/reference/Rhat.html)), a high
value for effective sample size (ESS), trace plots to look “grassy” or
“caterpillar like,” and posterior distributions to look relatively
normal.

## Model

We can now run the standard point process/detection probability model.

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
#> Chain 1: Gradient evaluation took 0.000967 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 9.67 seconds.
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
#> Chain 1:  Elapsed Time: 12.964 seconds (Warm-up)
#> Chain 1:                12.452 seconds (Sampling)
#> Chain 1:                25.416 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'COA_Standard_gaussian' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.00096 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 9.6 seconds.
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
#> Chain 2:  Elapsed Time: 11.924 seconds (Warm-up)
#> Chain 2:                10.856 seconds (Sampling)
#> Chain 2:                22.78 seconds (Total)
#> Chain 2: 
#>         warmup sample
#> chain:1 12.964 12.452
#> chain:2 11.924 10.856
```

We can inspect the object created with the first object containing the
Stan model. To view and work with the model itself call `m$model`.
However, as you will see there Are a bunch of other objects in the
objected created. These objects have pulled important information from
the Stan model.

``` r

summary(m)
#>                      Length Class         Mode   
#> model                 1     stanfit       S4     
#> summary              20     -none-        numeric
#> time                  1     -none-        numeric
#> summary_draws         4     draws_summary list   
#> coas                  8     tbl_df        list   
#> all_estimates        24     draws_df      list   
#> loc_draws             8     tbl_df        list   
#> param_draws          10     tbl_df        list   
#> generated_quantities  1     -none-        list
```

Let’s look at our trace plots for the model parameters and posterior
distributions to ensure the model converged properly. Remember the trace
plots should look grassy or caterpillar like. A trace plot is the
posterior draw for a given iteration plotted with the iteration number
on the x axis and the posterior value for a given parameter or latent
variable on the y. We evaluate this for both chains and want to see that
both chains are converging on a smiler posterior draw for a given
parameter or latent variable. We will first look at parameters of the
model.

``` r

stan_trace(m$model, pars = c("alpha0", "alpha1", "p0", "sigma"))
```

![](figure/trace%20plots%20and%20post%20dist-1.png)

``` r


stan_dens(
  m$model,
  pars = c("alpha0", "alpha1", "p0", "sigma"),
  separate_chains = TRUE,
  linewidth = 0.1
)
```

![](figure/trace%20plots%20and%20post%20dist-2.png)

We can see our trace plots look good and that the posterior
distributions of our paramaters look good.

Next let’s look at our latent variables which are `sx` and `sy` or the
estimated locations of the fish based on the detection probability. For
large/lengthy time periods it will become cumbersome to evaluate the
posteriors this way and we recommend inspecting $`\hat R`$ and ESS.

``` r

stan_trace(m$model, pars = c("sx[1,1]", "sx[1,2]", "sy[1,1]", "sy[1,2]"))
```

![](figure/trace%20plots%20and%20post%20dist%20latent-1.png)

``` r


stan_dens(
  m$model,
  pars = c("sx[1,1]", "sx[1,2]", "sy[1,1]", "sy[1,2]"),
  separate_chains = TRUE,
  linewidth = 0.1
)
```

![](figure/trace%20plots%20and%20post%20dist%20latent-2.png) Again, we
can see that everything looks good and our model has converged.

Now moving on to the other elements in the outputted object from
`COA_*()`.

The second element is a table of parameter estimates, `p0` which is the
estimated detection probability at distance of 0 m and $`\sigma`$ which
is units of distance and characterizes the effective spatial range of
detection: it is the standard deviation of the Gaussian or logistic
decay in detection log-odds with distance from the activity center. A
larger $`\sigma`$ corresponds to a more slow-decaying (longer-range)
detection function (`sigma`). The table contains their mean, standard
error of the mean, and the associated quantiles from the posterior
distributions. The table also includes the effective sample size and the
$`\hat R`$ statistic (which should be between 0.95 and 1.05).

``` r

m$summary
#>            mean     se_mean         sd      2.5%       25%       50%       75%     97.5%    n_eff     Rhat
#> p0    0.5018673 0.000965220 0.01893527 0.4670082 0.4887808 0.5021665 0.5152408 0.5360255 384.8492 1.003026
#> sigma 0.9926248 0.001377877 0.02835745 0.9404584 0.9723176 0.9916287 1.0118301 1.0472763 423.5588 1.004424
```

We can see that the mean detection probability at a distance of 0 m is
0.5 or 50% and that the model converged well for these parameters.

The thrid element returned, is the time required to run the model in
minutes. Note that Stan will automatically detect and use multiple
cores. If the computer used to run this has multiple cores, the time
returned will be longer than the actual run time. This is because it
will sum the time for each core. To return the realized run time, divide
`m$time` by the number of cores.

``` r

m$time
#> [1] 0.8032667
```

The fourth element returned, is a summary of the posterior draws.
Returned is the `variable`, `median`, and the 2.5 and 97.5% quartiles
(e.g., `q2.5` and `q97.5`). This summary `data.frame` provides you the
ability to inspect all elements, however, most of the time you will be
using this data in a filtered and transformed manner created in other
objects in the model output. These objects are more user friendly for
plotting and understanding the results.

``` r

m$summary_draws
#> # A tibble: 21 × 4
#>    variable   median   q2.5  q97.5
#>    <chr>       <dbl>  <dbl>  <dbl>
#>  1 alpha0    0.00867 -0.132  0.144
#>  2 alpha1    0.508    0.456  0.565
#>  3 sx[1,1]  -2.96    -3.28  -2.67 
#>  4 sx[1,2]  -2.96    -3.30  -2.61 
#>  5 sx[1,3]  -2.12    -2.30  -1.91 
#>  6 sx[1,4]  -2.26    -2.48  -2.04 
#>  7 sx[1,5]  -2.77    -3.09  -2.47 
#>  8 sx[1,6]  -2.29    -2.52  -2.05 
#>  9 sx[1,7]  -2.28    -2.48  -2.07 
#> 10 sx[1,8]  -2.98    -3.25  -2.73 
#> # ℹ 11 more rows
```

The fifth element returned, is `data.frame` of the median values for the
latent variables `sx` and `sy` which are the estimated location of the
fish. The median values can be viewed as the center of activity while
the posterior distribution is the overall probability of occurrence
around that estimate. The `data.frame` has a few important components
`ind` is the index number of the individual, `time` is the index number
for a given time bin, `x` and `y` are the median values for a given
individual and time, while `x_*` and `y_*` are the 2.5 and 97.5%
quantiles for those posteriors, this effectively is viewed as the 95%
credible interval, (i.e., Bayesian version of a confidence interval) for
each coordinate.

``` r

m$coas
#> # A tibble: 8 × 8
#>     ind  time     x       y x_lower x_upper y_lower y_upper
#>   <int> <int> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1     1     1 -2.96 -0.518    -3.28   -2.67 -0.882  -0.179 
#> 2     1     2 -2.96 -0.331    -3.30   -2.61 -0.699   0.0925
#> 3     1     3 -2.12  0.0652   -2.30   -1.91 -0.182   0.249 
#> 4     1     4 -2.26  0.144    -2.48   -2.04 -0.0993  0.396 
#> 5     1     5 -2.77 -0.273    -3.09   -2.47 -0.578   0.0535
#> 6     1     6 -2.29 -0.715    -2.52   -2.05 -0.987  -0.461 
#> 7     1     7 -2.28 -0.0781   -2.48   -2.07 -0.279   0.125 
#> 8     1     8 -2.98 -0.473    -3.25   -2.73 -0.766  -0.184
```

The sixth element returned, is a `data.frame` containing the posterior
draws from each non-warm-up iteration from all chains. This contains the
posterior distribution for each parameter and latent variable for each
individual in each time step. It is unlikely that you will use this
object instead we have created further objects that organize this data
for specific applications that you are more likely to use.

``` r

m$all_estimates
#> # A draws_df: 200 iterations, 2 chains, and 21 variables
#>     alpha0 alpha1 sx[1,1] sx[1,2] sx[1,3] sx[1,4] sx[1,5] sx[1,6]
#> 1   0.0491   0.52    -2.9    -2.9    -2.3    -2.2    -2.7    -2.3
#> 2   0.1010   0.51    -3.1    -2.8    -2.1    -2.1    -2.8    -2.2
#> 3   0.0498   0.52    -3.2    -2.6    -2.2    -2.1    -2.8    -2.1
#> 4   0.0020   0.51    -2.8    -3.0    -2.2    -2.1    -2.9    -2.2
#> 5  -0.0043   0.49    -2.9    -3.1    -2.1    -2.2    -3.0    -2.2
#> 6  -0.0358   0.47    -2.9    -3.0    -2.1    -2.3    -2.6    -2.3
#> 7   0.0061   0.53    -2.8    -3.1    -2.1    -2.2    -2.9    -2.2
#> 8   0.0050   0.50    -2.7    -3.0    -2.2    -2.3    -2.7    -2.3
#> 9  -0.0139   0.49    -2.9    -3.0    -2.2    -2.1    -2.9    -2.4
#> 10  0.0076   0.49    -2.9    -3.0    -2.2    -2.0    -2.9    -2.3
#> # ... with 390 more draws, and 13 more variables
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

The seventh element returned is a `data.frame` that contains the
extracted and transformed posterior draws for the latent variables `sx`
and `sy`. This object we will use to plot our estimates of centers of
activity. There are several other columns besides, the draws (i.e, `x`,
`y`), the `fish`/ind index number and the `time` index number. We have
`.chain`, `.iteration`, `.draw` which identifies the chain, iteration,
and draw that posterior draw came from. We also have the column `lp__`
which is the unnormalized log posterior density of your model, used for
diagnosing sampling efficiency, estimating approximations, and comparing
models.

Later on we will plot this object.

``` r

loc_draws <- m$loc_draws
loc_draws
#> # A tibble: 3,200 × 8
#>    .chain .iteration .draw   lp__  fish  time     x      y
#>     <int>      <int> <int>  <dbl> <int> <int> <dbl>  <dbl>
#>  1      1          1     1 -1279.     1     1 -2.85 -0.723
#>  2      1          1     1 -1279.     1     2 -2.87 -0.175
#>  3      1          1     1 -1279.     1     3 -2.27  0.129
#>  4      1          1     1 -1279.     1     4 -2.20  0.262
#>  5      1          1     1 -1279.     1     5 -2.75 -0.190
#>  6      1          1     1 -1279.     1     6 -2.26 -0.518
#>  7      1          1     1 -1279.     1     7 -2.32 -0.184
#>  8      1          1     1 -1279.     1     8 -2.95 -0.400
#>  9      1          2     2 -1281.     1     1 -3.07 -0.440
#> 10      1          2     2 -1281.     1     2 -2.80 -0.338
#> # ℹ 3,190 more rows
```

The eighth element returned, is a `data.frame` that contains the
contains the extracted and transformed posterior draws for the model
parameters `alpha0`, `alpah1`, `p0`, and `sigma`. There are several
other columns besides, the draws, the `fish`/ind index number and the
`time` index number. We have `.chain`, `.iteration`, `.draw` which
identifies the chain, iteration, and draw that posterior draw came from.
We also have the column `lp__` which is the unnormalized log posterior
density of your model, used for diagnosing sampling efficiency,
estimating approximations, and comparing models.

Later on we will plot this object.

``` r

param_draws <- m$param_draws
param_draws
#> # A tibble: 6,400 × 10
#>    .chain .iteration .draw   lp__  fish  time alpha0 alpha1    p0 sigma
#>     <int>      <int> <int>  <dbl> <int> <int>  <dbl>  <dbl> <dbl> <dbl>
#>  1      1          1     1 -1279.     1     1 0.0491  0.519 0.512 0.982
#>  2      1          1     1 -1279.     1     2 0.0491  0.519 0.512 0.982
#>  3      1          1     1 -1279.     1     3 0.0491  0.519 0.512 0.982
#>  4      1          1     1 -1279.     1     4 0.0491  0.519 0.512 0.982
#>  5      1          1     1 -1279.     1     5 0.0491  0.519 0.512 0.982
#>  6      1          1     1 -1279.     1     6 0.0491  0.519 0.512 0.982
#>  7      1          1     1 -1279.     1     7 0.0491  0.519 0.512 0.982
#>  8      1          1     1 -1279.     1     8 0.0491  0.519 0.512 0.982
#>  9      1          1     1 -1279.     1     1 0.0491  0.519 0.512 0.982
#> 10      1          1     1 -1279.     1     2 0.0491  0.519 0.512 0.982
#> # ℹ 6,390 more rows
```

The ninth element returned is a `list` that contains generated
quantities for `y` from the model. This `y` is different than `sy` and
is the estimated counts of detections for each individual at each time
step at each receiver. The object `yrep` which is a 2-dimensional array
that contains 10 draws (`ndraws` argument controls the number of draws
here) and 640 posterior counts of detections the tag number, receiver
number and time bin. We will assess to see how well this matches the
existing data’s distribution as using [posterior
probability](https://en.wikipedia.org/wiki/Posterior_probability) which
is a type of conditional probability that results from updating the
prior probability with information summarized by the likelihood.

``` r

yrep <- m$generated_quantities
str(yrep)
#> List of 1
#>  $ yrep: int [1:10, 1:640] 1 2 2 0 4 0 0 2 0 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "yrep_1" "yrep_2" "yrep_3" "yrep_4" ...
#>   .. ..$ : chr [1:640] "tag_1_rec_1_time_1" "tag_1_rec_2_time_1" "tag_1_rec_3_time_1" "tag_1_rec_4_time_1" ...
```

## Plotting

First, we are going to plot the posterior draws for locations. Within
the package we have a `sf` object that is the shape of Parry Sound. We
can use this to plot the posterior draws of `sx` and `sy` to understand
the movement of that of lake trout for 8 hours.

We need to take that `sf` object and transform it into an aeqd
projection.

``` r

ps_aeqd <- ps |>
  st_transform(aeqd_crs)
```

Now we can plot this within in our system both combined and broken up by
time.

``` r

p <- ggplot() +
  geom_sf(data = ps_aeqd) +
  geom_hex(data = loc_draws, aes(x = x, y = y), bins = 50) +
  scale_fill_viridis_c(option = "H", name = "Posterior\nDensity") +
  scale_x_continuous(n.breaks = 4) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank()
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

p1 <- p +
  facet_wrap(~time)
```

Let’s first look at the posterior densities for each time step

``` r

p1
```

![](figure/post%20densities%20timestep-1.png)

Next let’s look at the densities combined to gather a better
understanding of the movement over the 8 timesteps.

``` r

p
```

![](figure/post%20densities%20combined-1.png)

Next we can plot the posterior distributions for `alpha0`, `alpha1`,
`p0`, and `sigma`. We need to first make `fish` and `time` a `character`
and then to make plotting easier we need to make the `data.frame` in
long format.

``` r

param_draws$fish <- as.character(param_draws$fish)
param_draws$time <- as.character(param_draws$time)

param_draws_long <- param_draws |>
  pivot_longer(
    cols = -c(".chain", ".iteration", ".draw", "lp__", "fish", "time"),
    names_to = "param",
    values_to = "est"
  )
```

We can now plot those posterior distributions of the model parameters.

``` r

p_param <- ggplot(
  data = param_draws_long,
  aes(x = time, y = est, fill = fish),
) +
  geom_violin() +
  facet_wrap(~param, scale = "free_y") +
  scale_fill_manual(name = "Fish ID", values = "#2768F5") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank()
  ) +
  labs(x = "Time Bin", y = "Estimate")

p_param
```

![](figure/plot%20param-1.png)

We can see that they are all quite tightly distributed with `p0`
indicating that detection probability at a distance of `0` is between
48 - 54 %, while the `sigma` is around 1 km.

Lastly, we can plot the predictive posterior check using
[tidybayes](https://mjskay.github.io/tidybayes/). First we need to make
detection counts as a vector from our `build_count()` object.

``` r

y_obs <- as.vector(ps_count_example[!is.na(ps_count_example)])
```

Next we can plot the densities using
[`ppc_dens_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
from [bayesplot](https://mc-stan.org/bayesplot/).

``` r

ppc <- ppc_dens_overlay(y = y_obs, yrep = yrep$yrep)
ppc
```

![](figure/ppc%20dens-1.png)

We can see our predictive posterior check distribution for 10 draws
closely lines up with our observed detection counts indicating that the
model fits the data well.

Congratulations we have successfully run a Bayesian standard point
processing/detection probability model for one Lake Trout for 8, 1 hour
time bins (8 hrs) in Parry Sound, a large embayment of Georgian Bay,
Lake Huron. We can take the results and write up how this individual
behaved for a given time period based on the model results!

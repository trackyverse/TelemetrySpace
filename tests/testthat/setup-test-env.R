# ----- lets setup using new data -----

# transform to utms

ps_rec_loc <- ps_rec_loc |>
  sf::st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)

ps_rec_loc_utm <- ps_rec_loc |>
  sf::st_transform(32617)

# ---- build aeqd ------
aeqd_crs <- build_aeqd(ps_rec_loc_utm)
# transform to aeqd
ps_rec_loc_aeqd <- ps_rec_loc |>
  sf::st_transform(aeqd_crs) |>
  (\(.) .[order(.$station_no), ])()

# ---- build pixel grid ----
ps_utm <- ps |>
  sf::st_transform(32617)


ps_pixel_grid <- build_pixel_grid(ps_utm, res = 500, crs = aeqd_crs)
# ---- index the receivers -----

ps_rec_loc_aeqd$rec <- 1:nrow(ps_rec_loc_aeqd)

# build receiver vectors
rec_loc_vec <- build_rec_coords(ps_rec_loc_aeqd)

# build receiver limits
rec_limits <- build_bbox(rec_loc_vec)

# ----- build time bins -----
ps_det_example_clean <- ps_det_example
ps_det_example <- build_time_bin(ps_det_example, unit = "1 hour")
ps_det_example_t <- build_time_bin(ps_det_example, unit = "1 hour")

ps_det_example <- ps_det_example[ps_det_example$time < 3, ]


# ----- merge receiver index with detection info -----

ps_det_example <- merge(
  ps_det_example,
  sf::st_drop_geometry(ps_rec_loc_aeqd),
  by = "station_no"
)
unique(ps_det_example$station_no)

# ----- build counts ------
ps_count_example <- build_counts(
  df = ps_det_example,
  nrec = nrow(ps_rec_loc_aeqd),
  rec_id = ps_rec_loc_aeqd$rec,
  rec_names = ps_rec_loc_aeqd$station_no
)
# ----- build timesteps -------
time_steps <- build_tstep(ps_count_example)

# ----- build nind and ntrans -----
nind <- length(unique(ps_det_example$tag_serial_no))

ntrans <- build_ntrans(ps_det_example)

# ------ create stand data to use to test the models ------
standata <- list(
  nind = nind,
  nrec = nrow(ps_rec_loc_aeqd),
  ntime = time_steps,
  ntrans = ntrans,
  y = ps_count_example,
  recX = rec_loc_vec$recX,
  recY = rec_loc_vec$recY,
  xlim = rec_limits$xlim,
  ylim = rec_limits$ylim
)


# --------------------- BUILD TEST TAG LOCS -------
ps_test_tag_loc_sf <- ps_test_tag_loc |>
  sf::st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) |>
  sf::st_transform(32617)


ps_test_tag_loc_aeqd <- ps_test_tag_loc_sf |>
  sf::st_transform(aeqd_crs)


test_tag_locs <- build_rec_coords(ps_test_tag_loc_aeqd)

# ----- build test data ----
# ---- build time bins -----

ps_det_test_tag <- build_time_bin(ps_det_test_tag, unit = "1 hour")

# ----- add in rec from receiver ----
ps_det_test_tag <- merge(
  ps_det_test_tag,
  sf::st_drop_geometry(ps_rec_loc_aeqd),
  by = "station_no"
) |>
  (\(.) .[order(.$time_bin), ])()


# ----- only select the first 2 times ----
ps_det_test_tag <- ps_det_test_tag[ps_det_test_tag$time < 3, ]
str(ps_det_test_tag)

ps_test_tag_count <- build_counts(
  df = ps_det_test_tag,
  nrec = nrow(ps_rec_loc_aeqd),
  rec_id = ps_rec_loc_aeqd$rec,
  rec_names = ps_rec_loc_aeqd$station_no
)


# ---- nsent ----
nsentinel <- length(unique(ps_det_test_tag$tag_serial_no))


standata_testtag <- list(
  nind = nind,
  nrec = nrow(ps_rec_loc_aeqd),
  ntime = time_steps,
  ntrans = ntrans,
  y = ps_count_example,
  recX = rec_loc_vec$recX,
  recY = rec_loc_vec$recY,
  xlim = rec_limits$xlim,
  ylim = rec_limits$ylim,
  ntest = nsentinel,
  test = ps_test_tag_count,
  testX = array(test_tag_locs$recX, dim = c(nsentinel)),
  testY = array(test_tag_locs$recY, dim = c(nsentinel)) # N-S b
)


# ----- intiatal value functions

init_fun <- function() {
  build_init(rec_loc_vec, nind = nind, tstep = time_steps)
}
# ----- run each model ------
# ----- standard coa ------
standard_gaussian <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 300,
      iter = 1000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)

standard_logistic <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 400,
      iter = 1200,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)
# ----- time integrated -----
# first make p0 names to test
ntime <- time_steps
nrec <- nrow(ps_rec_loc_aeqd)

p0_names <- outer(seq_len(ntime), seq_len(nrec), FUN = function(i, j) {
  sprintf("p0[%d,%d]", i, j)
})

# column-major (Stan default): first index varies fastest
p0_names <- sprintf(
  "p0[%d,%d]",
  rep(seq_len(ntime), times = nrec),
  rep(seq_len(nrec), each = ntime)
)


# ----- run model
time_vary_gaussian <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 300,
      iter = 1000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)


time_vary_logistic <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 400,
      iter = 1100,
      # warmup = 3000,
      # iter = 7000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)

# ----- tag integraged -----
tag_int_gaussian <- do.call(
  COA_TagInt,
  c(
    standata_testtag,
    list(
      chains = 2,
      warmup = 400,
      iter = 1000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "gaussian"
    )
  )
)
tag_int_logistic <- do.call(
  COA_TagInt,
  c(
    standata_testtag,
    list(
      chains = 2,
      warmup = 400,
      iter = 1000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11,
      init = init_fun,
      decay = "logistic"
    )
  )
)

# Extract Draws

These functions allow draws to be extracted from `{posterior}` objects.

## Usage

``` r
extract_coa(summary_draws)

extract_d_probs(summary_draws)

extract_loc_draws(draws)

extract_param_draws(draws)
```

## Arguments

- summary_draws:

  a `draws_summary` object from
  [`posterior::summarize_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)

- draws:

  a `draws_df` object from
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)

## Value

`extract_coa()` - returns a `data.frame` containing the median and the
2.5, and 97.5% quantiles.

`extract_d_probs()` - returns a `data.frame` containing the median and
the 2.5, and 97.5% quantiles.

`extract_loc_draws()` - returns a `data.frame` containing the following
columns: `.chain`, `.iteration`, `.draw`, `lp__`, `fish`, `time`, `x`,
and `y`.

`extract_param_draws()` - returns a `data.frame` containing the
following columns: `.chain`, `.iteration`, `.draw`, `lp__` and then
posterior draws for the paramaters of the detection probablity likihood
(i.e., `alpha0` and `alpha1`) and generated quantiteies (i.e., `p0`).

## Details

`extract_coa()` - extracts median and the 2.5, and 97.5% quantiles for
posterior draws of `sx` and `sy`, which is the estiamted center of
activity for a given individual within a given time bin.

`extract_d_probs()` - extracts median and the 2.5, and 97.5% quantiles
for posterior draws of `p0` which is the detection probablity at
distance 0, used when estimated in time varying and tag integrated
models.

`extract_loc_draws()` - extracts posterior draws for the latent
variables `sx` and
``` sy`` for each fish at each time bin from  ```draws_df`object and transforms it so that the fish number, time, and draw are in a`data.frame`. This can then be further ploted or transformed into a `sf\`
object.

`extract_param_draws()` - extracts posterior draws for the detection
intercept (i.e., logit scale; `alpha0`), the distance-decay coefficient
(i.e., `alpha1`), any other coefficents, and the detction probablity at
distance 0 (i.e. `p0`) from `draws_df` object.

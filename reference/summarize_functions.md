# Summarize Posterior Draws

Summarize Posterior Draws

## Usage

``` r
summarize_draws(draws)
```

## Arguments

- draws:

  a `draws_df` object from
  [`posterior::as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)

## Value

returns a summarized dataframe with the median and 2.5% and 97.5%
quantitles.

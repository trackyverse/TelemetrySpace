# Save this file as `R/COA_standard.R`

#' Fits a Bayesian Spatial Point Process model to estimate individual centers of activity from acoustic telemetry data using Stan
#'

#' @param nind   Number of tagged individuals
#' @param nrec   Number of receivers
#' @param ntime  Number of time steps
#' @param ntrans Number of expected transmissions per tag per time interval
#' @param y      Array of detection data, where row = individual, column = time step, and matrix = receiver
#' @param recX   Receiver coordinates in the east-west direction (should be projected and scaled for computational efficiency)
#' @param recY   Receiver coordinates in the north-south direction (should be projected and scaled for computational efficiency)
#' @param xlim   East-west boundaries of spatial extent (receiver array + buffer)
#' @param ylim   North-south boundaries of spatial extent (receiver array + buffer).
#' @param decay  desired decay function. Currently one of "gaussian" or "logistic". Default is "gaussian".
#' @param ndraws to be passed to `generated_quantities`. Changes the number of draws. Default is 10.
#' @param ... Additional arguments passed to `sampling` from `rstan`.
#' This can include setting `chains`, `iter`, `warmup`, and `control`. Please see
#' `rstan::sampling()` for more info.
#'
#' @return COA_Standard returns an object of class `stanfit` returned by `rstan::sampling`. See the `rstan` package documentation for details.
#' @return This function returns a list containing the following components: 1) a summary of the detection function parameters; 2) the time required for model fitting; 3) the estimated COAs for each individual in each time step and 95 percent credible interval; and 4) a dataframe containing values for each parameter and latent parameter from chain iterations. These can be used to plot posterior distributions and the credible interval around each estimated COA.
#' @seealso [rstan::sampling()]
#'
#'
#' @export
COA_Standard <- function(
  nind,
  nrec,
  ntime,
  ntrans,
  y,
  recX,
  recY,
  xlim,
  ylim,
  decay = "gaussian",
  ndraws = NULL,
  ...
) {
  # First move everything into a list
  standata <- list(
    nind = nind,
    nrec = nrec,
    ntime = ntime,
    ntrans = ntrans,
    y = y,
    recX = recX,
    recY = recY,
    xlim = xlim,
    ylim = ylim
  )
  # validate this list prior to sending it to the model
  exp_len <- expected_lengths(recX = recX, recY = recY)

  validate_standata(standata, exp_len)

  # fit model
  if (decay == "gaussian") {
    fit_model <- rstan::sampling(
      stanmodels$COA_Standard_gaussian,
      data = standata,
      ...
    )
  } else if (decay == "logistic") {
    fit_model <- rstan::sampling(
      stanmodels$COA_Standard_logistic,
      data = standata,
      ...
    )
  } else {
    cli::cli_abort(
      "{.arg decay} must be one of {.code 'gaussian' or 'logistic'}."
    )
  }

  # Save chains after discarding warmup
  fit_draws <- posterior::as_draws_df(fit_model)
  # Note this returns parameters and latent states/derived values

  # Summary statistics and convergence diagnostics
  if (decay == "gaussian") {
    fit_summary <- rstan::summary(fit_model, pars = c("p0", "sigma"))$summary
  } else if (decay == "logistic") {
    fit_summary <- rstan::summary(fit_model, pars = c("p0"))$summary
  }

  # How much time did fitting take?
  fit_time <- sum(print(rstan::get_elapsed_time(fit_model))) / 60

  # calculate generated quantities
  fit_generated_quantities <- generated_quantities(
    model = fit_model,
    standata = standata,
    ndraws = ndraws
  )
  # transform gq into matrix
  tran_fit_gq <- transform_gq(fit_generated_quantities)
  # Extract COA estimates
  summary_draws <- summarize_draws(fit_draws)

  coas <- extract_coa(summary_draws)

  # extract location and paramater draws
  loc_draws <- extract_loc_draws(fit_draws)

  param_draws <- extract_param_draws(fit_draws)

  # Report results
  model_results <- list(
    fit_model,
    fit_summary,
    fit_time,
    summary_draws,
    coas,
    fit_draws,
    loc_draws,
    param_draws,
    tran_fit_gq
  )
  names(model_results) <- c(
    'model',
    'summary',
    'time',
    "summary_draws",
    'coas',
    'all_draws',
    'loc_draws',
    'param_draws',
    'generated_quantities'
  )
  return(model_results)
}

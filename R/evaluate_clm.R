#' Evaluate models with outc ~ expo*decades_since_start
#'
#' @param fit A model object fitted with `ordinal::clm()`
#' @param n_sample Number of samples from predicted probabilities, default is
#' 100
#' @param fixed_expo_perc A numeric vector containing the exposure values for
#' which the decades effect estimate is caluclated. Values are numeric and
#' between 0 and 1. Values must be listed in ascending order.
#' @param data The data.frame passed to `ordinal::clm()`
#' @param expo_mean Mean of exposure (TUD or CBC) used for standardisation,
#' i.e., tud_mean_srh, tud_mean_swl, cbc_mean_srh, or cbc_mean_swl, depending on
#' exposure and outcome
#' @param expo_sd SD of exposure (TUD or CBC) used for standardisation, i.e.,
#' tud_sd_srh, tud_sd_swl, cbc_sd_srh, or cbc_sd_swl depending on exposure and
#' outcome
#' @param seed Random seed
#' @param predict Boolean value, which indicates if predictions are calculated
#' (default is TRUE)
#'
#' @returns A list with the fit and all evaluation objects
#' @export
#'
#' @examples
#' f <- ordinal::clm(
#'   swl ~ tud * decades_since_start,
#'   nominal = ~country,
#'   data = test_data,
#'   link = "logit"
#' )
#'
#' tud_mean <- test_data$tud_original |> mean()
#' tud_sd <- test_data$tud_original |> sd()
#'
#' evaluate_clm(
#'   fit = f,
#'   n_sample = 3,
#'   data = test_data,
#'   expo_mean = tud_mean,
#'   expo_sd = tud_sd,
#'   seed = 57733
#' )
evaluate_clm <- function(
  fit,
  n_sample = 100,
  fixed_expo_perc = c(0.20, 0.40, 0.60, 0.80),
  data,
  expo_mean,
  expo_sd,
  seed,
  predict = TRUE
) {
  # Check input
  rlang::try_fetch(
    checkmate::assert_class(fit, "clm", null.ok = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        "Argument `fit` must be an object created by `ordinal::clm()`.",
        parent = NA
      )
    }
  )
  if (
    !identical(
      fit$formulas$nominal |>
        as.list() |>
        purrr::map(rlang::as_string) |>
        unlist(),
      c("~", "country")
    )
  ) {
    cli::cli_abort(
      "`fit` is expected to have cut-points that vary by country.",
      class = "error.cutpoints.by.country",
      parent = NA
    )
  }
  checkmate::assert_numeric(
    fixed_expo_perc,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    sorted = TRUE,
    null.ok = FALSE
  )
  checkmate::assert_numeric(
    expo_mean,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_numeric(
    expo_sd,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    n_sample,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    seed,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_flag(
    predict,
    na.ok = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_data_frame(data, null.ok = FALSE)
  rlang::try_fetch(
    {
      checkmate::assert_subset(
        c("country", "year", "class", "c", "wave"),
        colnames(data),
        empty.ok = FALSE
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        paste0(
          "Argument `data` must contain variables `country`, `year`, `class`,",
          " `c`, and `wave`."
        ),
        parent = NA
      )
    }
  )

  # Get exposure and outcome
  expo <- get_exposure(fit)
  outc <- get_outcome(fit)

  # Coefficients
  coefs <- fit |>
    coefs_clm(expo_mean = expo_mean, expo_sd = expo_sd)

  # Predictions
  if (predict) {
    ## Create newdata
    newdata <- create_newdata(
      fit = fit,
      fixed_expo_perc = fixed_expo_perc,
      expo_mean = expo_mean,
      expo_sd = expo_sd
    )

    ## Get predictions
    pred <- get_predictions(
      fit = fit,
      newdata = newdata
    )

    ## Draw samples
    samples <- draw_samples(
      fit = fit,
      pred = pred,
      n_sample = n_sample,
      seed = seed
    )

    ## Summarise samples
    summ <- summarise_samples(
      fit = fit,
      samples = samples
    )

    ## Combine samples
    pred_results <- combine_summaries(
      summ = summ,
      data = data
    )
  } else {
    pred_results <- NA
  }

  # Return
  list(
    fit = fit,
    nobs = stats::nobs(fit),
    coefs = coefs,
    pred = pred_results
  )
}

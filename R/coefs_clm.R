#' Extract coefficient tables for models with outc ~ expo*decades_since_start
#'
#' @param fit A fitted model object
#' @param expo_mean Mean of exposure (TUD or CBC) used for standardisation,
#' i.e., tud_mean_srh, tud_mean_swl, cbc_mean_srh, or cbc_mean_swl, depending on
#' exposure and outcome
#' @param expo_sd SD of exposure (TUD or CBC) used for standardisation, i.e.,
#' tud_sd_srh, tud_sd_swl, cbc_sd_srh, or cbc_sd_swl depending on exposure and
#' outcome
#' @param fixed_year x
#' @param fixed_expo_perc x
#'
#' @returns A data.frame
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
#' coefs_clm(
#'   fit = f,
#'   expo_mean = tud_mean,
#'   expo_sd = tud_sd
#' )
coefs_clm <- function(
  fit,
  expo_mean,
  expo_sd,
  fixed_year = c(1985, 1995, 2005, 2015),
  fixed_expo_perc = c(0.20, 0.40, 0.60, 0.80)
) {
  # Check input
  rlang::try_fetch(
    checkmate::assert_class(fit, "clm", null.ok = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        "Argumnet `fit` must be an object created by `ordinal::clm()`.",
        parent = NA
      )
    }
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
    fixed_year,
    lower = 1980,
    upper = 2025,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    sorted = TRUE,
    null.ok = FALSE
  )
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

  # Get exposure
  expo <- get_exposure(fit)
  param_inter <- paste0(expo, ":decades_since_start")

  # Get coefficient from model
  coefs <- broom::tidy(fit) |>
    dplyr::filter(.data[["coef.type"]] == "location")

  # Get variance/covariance
  v <- stats::vcov(fit)
  var_expo <- v[expo, expo]
  var_dec <- v["decades_since_start", "decades_since_start"]
  var_interaction <- v[param_inter, param_inter]
  cov_expo_interaction <- v[expo, param_inter]
  cov_dec_interaction <- v["decades_since_start", param_inter]

  # Convert input values
  fixed_dec <- fixed_year |>
    magrittr::subtract(1981) |>
    magrittr::divide_by(10)

  fixed_expo <- fixed_expo_perc |>
    magrittr::subtract(expo_mean) |>
    magrittr::divide_by(expo_sd)

  # Calculate estimates at fixed values
  ## expo
  est_expo <- fixed_dec |>
    purrr::map(\(x) {
      coefs[coefs$term == expo, "estimate"] +
        x * coefs[coefs$term == param_inter, "estimate"]
    })

  se_expo <- fixed_dec |>
    purrr::map(\(x) {
      sqrt(var_expo + x^2 * var_interaction + 2 * x * cov_expo_interaction)
    })

  ## dec
  est_dec <- fixed_expo |>
    purrr::map(\(x) {
      coefs[coefs$term == "decades_since_start", "estimate"] +
        x * coefs[coefs$term == param_inter, "estimate"]
    })

  se_dec <- fixed_expo |>
    purrr::map(\(x) {
      sqrt(var_dec + x^2 * var_interaction + 2 * x * cov_dec_interaction)
    })

  # Fill coefs
  ## expo
  for (i in seq_len(length(fixed_year))) {
    coefs <- coefs |>
      dplyr::rows_append(
        tibble::tibble(
          term = paste0(
            expo |> stringi::stri_trans_toupper(),
            " (",
            fixed_year[i],
            ")"
          ),
          estimate = est_expo[[i]][[1]],
          std.error = se_expo[[i]][[1]],
          coef.type = "inter_expo"
        )
      )
  }
  rm(i)

  ## dec
  for (i in seq_len(length(fixed_expo_perc))) {
    coefs <- coefs |>
      dplyr::rows_append(
        tibble::tibble(
          term = paste0(
            "Decade (",
            expo |> stringi::stri_trans_toupper(),
            "=",
            fixed_expo_perc[i] |>
              magrittr::multiply_by(100) |>
              round(2),
            "%)"
          ),
          estimate = est_dec[[i]][[1]],
          std.error = se_dec[[i]][[1]],
          coef.type = "inter_dec"
        )
      )
  }
  rm(i)

  # Remove initial parameters and exponentiate
  coefs |>
    dplyr::filter_out(
      .data[["term"]] %in% c(expo, "decades_since_start", param_inter)
    ) |>
    dplyr::mutate(
      "lwr" := .data[["estimate"]] - 1.96 * .data[["std.error"]],
      "upr" := .data[["estimate"]] + 1.96 * .data[["std.error"]],
      "estimate" := exp(.data[["estimate"]]),
      "lwr" := exp(.data[["lwr"]]),
      "upr" := exp(.data[["upr"]])
    ) |>
    dplyr::select(
      dplyr::all_of(c("term", "estimate", "lwr", "upr", "coef.type"))
    )
}

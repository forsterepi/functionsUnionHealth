#' Get exposure
#'
#' Evaluate if the exposure of interest is "tud" (trade union density) or "cbc"
#' (collective bargaining coverage)
#'
#' @param fit An object of class `clm` created by `ordinal::clm()`.
#'
#' @returns A string, either "tud" or "cbc"
#'
#' @noRd
get_exposure <- function(fit) {
  # Check input
  rlang::try_fetch(
    checkmate::assert_class(fit, "clm", null.ok = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        "Argumnet `fit` must be an object created by `ordinal::clm()`.",
        class = "errorclass",
        parent = NA
      )
    }
  )

  # Get correct exposure
  if ("tud:decades_since_start" %in% names(fit$coefficients)) {
    return("tud")
  }
  if ("cbc:decades_since_start" %in% names(fit$coefficients)) {
    return("cbc")
  }

  # Throw error if no condition is fulfilled
  cli::cli_abort(paste0(
    "There is neither `tud:decades_since_start` nor",
    "`cbc:decades_since_start` is `fit$model`."
  ))
}

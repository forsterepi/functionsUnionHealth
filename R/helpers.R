#' Get exposure
#'
#' Evaluate if the exposure of interest is "tud" (trade union density) or "cbc"
#' (collective bargaining coverage)
#'
#' @param fit An object of class `clm` created by `ordinal::clm()`
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

#' Get outcome
#'
#' Evaluate if the outcome of interest is "srh" (self-rated health) or "swl"
#' (satisfaction with life)
#'
#' @param fit An object of class `clm` created by `ordinal::clm()`
#'
#' @returns A list with elements `type`, `int`, and `colnames`. `type` is either
#' "srh" or "swl". `int` is a vector of integerish numbers representing the
#' factor levels of the outcome factor. It is `1:4` for "srh" and `1:5` for
#' "swl". `colnames` contains a vector of the factor labels of the outcome
#' processed with `make.names()` so they can be used as variable names.
#'
#' @noRd
get_outcome <- function(fit) {
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

  # Get correct outcome
  if (identical(fit$formula[[2]], rlang::sym("srh"))) {
    return(
      list(
        type = "srh",
        int = 1:(fit$model$srh |> nlevels()),
        colnames = fit$model$srh |>
          levels() |>
          stringi::stri_trans_tolower() |>
          make.names()
      )
    )
  }
  if (identical(fit$formula[[2]], rlang::sym("swl"))) {
    return(
      list(
        type = "swl",
        int = 1:(fit$model$swl |> nlevels()),
        colnames = fit$model$swl |>
          levels() |>
          stringi::stri_trans_tolower() |>
          make.names()
      )
    )
  }

  # Throw error if no condition is fulfilled
  cli::cli_abort(paste0(
    "The outcome of model `fit` is neither `srh` nor `swl`."
  ))
}

#' Create newdata data.frames for use in `predict()`
#'
#' @param fit A model object fitted with `ordinal::clm()`
#' @param fixed_expo_perc A numeric vector containing the exposure values for
#' which the decades effect estimate is caluclated. Values are numeric and
#' between 0 and 1. Values must be listed in ascending order.#
#' @param expo_mean Mean of exposure (TUD or CBC) used for standardisation,
#' i.e., tud_mean_srh, tud_mean_swl, cbc_mean_srh, or cbc_mean_swl, depending on
#' exposure and outcome
#' @param expo_sd SD of exposure (TUD or CBC) used for standardisation, i.e.,
#' tud_sd_srh, tud_sd_swl, cbc_sd_srh, or cbc_sd_swl depending on exposure and
#' outcome
#'
#' @noRd
create_newdata <- function(fit, fixed_expo_perc, expo_mean, expo_sd) {
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

  # Get exposure and outcome
  expo <- get_exposure(fit)
  outc <- get_outcome(fit)

  # Get fixed expo values
  fixed_expo <- fixed_expo_perc |>
    magrittr::subtract(expo_mean) |>
    magrittr::divide_by(expo_sd)

  # Prepare newdata0_proto
  newdata0_proto <- fit$model |>
    dplyr::select(!dplyr::all_of(c(expo, outc$type)))

  # Create newdata
  fit$model$country |>
    unique() |>
    as.character() |>
    purrr::map(\(input_c) {
      # Create predictions with observed values apart from TUD
      ## Get data used for modelling for all variables but outcome and TUD
      newdata0 <- newdata0_proto |>
        dplyr::filter(.data[["country"]] == input_c)

      ## Add different TUD value to that data
      newdata <- fixed_expo |>
        purrr::map(\(x) {
          newdata0 |>
            dplyr::mutate("{expo}" := x)
        })
    })
}

#' Get predictions for `evaluate_clm()`
#'
#' @param fit A model object fitted with `ordinal::clm()`
#' @param newdata Output from `create_newdata()`
#'
#' @noRd
get_predictions <- function(fit, newdata) {
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
  checkmate::assert_list(
    newdata,
    types = "list",
    any.missing = FALSE,
    min.len = 1,
    null.ok = FALSE
  )
  newdata |>
    purrr::map(\(x) {
      x |>
        purrr::map(
          checkmate::assert_data_frame,
          min.rows = 1,
          min.cols = 3,
          null.ok = FALSE
        )
    })

  # Get exposure and outcome
  expo <- get_exposure(fit)
  outc <- get_outcome(fit)

  # Create and fill pred
  pred <- vector(mode = "list", length = length(newdata))

  for (i in seq_along(newdata)) {
    pred[[i]] <- newdata[[i]] |>
      purrr::map(\(x) {
        stats::predict(fit, newdata = x, interval = FALSE) |>
          magrittr::extract2("fit") |>
          as.data.frame() |>
          magrittr::set_colnames(outc$colnames) |>
          vctrs::vec_cbind(
            x |>
              dplyr::select(dplyr::all_of(c(
                "decades_since_start",
                "country",
                expo
              )))
          )
      })
  }

  # Return
  pred
}

#' Draw samples from predictions for `evaluate_clm()`
#'
#' @param fit A model object fitted with `ordinal::clm()`
#' @param pred Output from `get_predictions()`
#' @param n_sample Number of samples from predicted probabilities, default is
#' 100
#' @param seed Random seed
#'
#' @noRd
draw_samples <- function(fit, pred, n_sample, seed) {
  # Check inputs
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
  checkmate::assert_list(
    pred,
    types = "list",
    any.missing = FALSE,
    min.len = 1,
    null.ok = FALSE
  )
  pred |>
    purrr::map(\(x) {
      x |>
        purrr::map(
          checkmate::assert_data_frame,
          min.rows = 1,
          min.cols = 3,
          null.ok = FALSE
        )
    })
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

  # Get outcome
  outc <- get_outcome(fit)

  # Create container
  samples <- seq_along(pred) |>
    purrr::map(\(x) {
      vector(mode = "list", length = length(pred[[x]]))
    })

  ## Set seed
  set.seed(seed)

  # Sample
  for (k in seq_along(samples)) {
    cat(paste0(
      "--- Drawing samples ",
      k,
      "/",
      length(samples),
      " ---\n"
    ))
    for (j in seq_along(pred[[k]])) {
      temp_samples <- vector(mode = "list", length = nrow(pred[[k]][[j]]))

      for (i in seq_along(temp_samples)) {
        temp_samples[[i]] <-
          sample(
            outc$int,
            size = n_sample,
            replace = TRUE,
            prob = pred[[k]][[j]][i, outc$colnames]
          ) |>
          matrix(nrow = 1) |>
          as.data.frame()
      }

      samples[[k]][[j]] <- pred[[k]][[j]] |>
        vctrs::vec_cbind(
          temp_samples |>
            purrr::list_rbind() |>
            magrittr::set_colnames(paste0("draw", 1:n_sample))
        )
    }
  }

  # Return
  samples
}

#' Summarise samples for `evaluate_clm()`
#'
#' @param fit An object of class `clm` created by `ordinal::clm()`
#' @param samples Output from `draw_samples()`
#'
#' @noRd
summarise_samples <- function(fit, samples) {
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
  checkmate::assert_list(
    samples,
    types = "list",
    any.missing = FALSE,
    min.len = 1,
    null.ok = FALSE
  )
  samples |>
    purrr::map(\(x) {
      x |>
        purrr::map(
          checkmate::assert_data_frame,
          min.rows = 1,
          min.cols = 3,
          null.ok = FALSE
        )
    })

  # Get exposure
  expo <- get_exposure(fit)

  # Create container
  summ <- seq_along(samples) |>
    purrr::map(\(x) {
      vector(mode = "list", length = length(samples[[x]]))
    })

  # Fill container
  for (k in seq_along(samples)) {
    for (j in seq_along(samples[[k]])) {
      summ[[k]][[j]] <- samples[[k]][[j]] |>
        dplyr::summarise(
          dplyr::across(dplyr::starts_with("draw"), mean),
          country = dplyr::first(.data[["country"]]),
          "{expo}" := dplyr::first(.data[[expo]]),
          .by = dplyr::all_of("decades_since_start")
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("draw"),
          names_to = "draw",
          values_to = "val"
        ) |>
        dplyr::summarise(
          mean_sampled = mean(.data[["val"]]),
          q025_sampled = stats::quantile(
            .data[["val"]],
            probs = 0.025,
            names = FALSE
          ),
          q975_sampled = stats::quantile(
            .data[["val"]],
            probs = 0.975,
            names = FALSE
          ),
          country = dplyr::first(.data[["country"]]),
          "{expo}" := dplyr::first(.data[[expo]]),
          .by = dplyr::all_of("decades_since_start")
        )
    }
  }

  # Return
  summ
}

#' Combine sample summaries for `evaluate_clm()`
#'
#' @param summ Output from `summarise_samples()`
#' @param data The data.frame passed to `ordinal::clm()`
#'
#' @noRd
combine_summaries <- function(summ, data) {
  # Check input
  checkmate::assert_list(
    summ,
    types = "list",
    any.missing = FALSE,
    min.len = 1,
    null.ok = FALSE
  )
  summ |>
    purrr::map(\(x) {
      x |>
        purrr::map(
          checkmate::assert_data_frame,
          min.rows = 1,
          min.cols = 3,
          null.ok = FALSE
        )
    })
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

  # Combine summ, add variables from data, and do some processing
  comb <- summ |>
    purrr::map(purrr::list_rbind) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      year = .data[["decades_since_start"]] |>
        magrittr::multiply_by(10) |>
        magrittr::add(1981) |>
        round(0)
    ) |>
    dplyr::left_join(
      data |>
        dplyr::select(dplyr::all_of(c(
          "country",
          "year",
          "class",
          "c",
          "wave"
        ))) |>
        plyr::count() |>
        dplyr::select(!dplyr::all_of("freq")),
      by = c("country", "year")
    ) |>
    dplyr::mutate(
      year = as.factor(.data[["year"]])
    )
}

#' Create comparison plot for multiple models
#'
#' @param ... Models created with `evaluate_clm()`
#' @param my_xlim A numeric vector of length 2 specifying xlim of the plot
#'
#' @returns A ggplot
#' @export
#'
#' @examples
#' f1 <- ordinal::clm(
#'   swl ~ cbc * decades_since_start,
#'   nominal = ~country,
#'   data = test_data,
#'   link = "logit"
#' )
#'
#' f2 <- ordinal::clm(
#'   swl ~ cbc * decades_since_start + age_group + gender + job,
#'   nominal = ~country,
#'   data = test_data,
#'   link = "logit"
#' )
#'
#' tud_mean <- test_data$tud_original |> mean()
#' tud_sd <- test_data$tud_original |> sd()
#'
#' ModF1 <- evaluate_clm(
#'   f1,
#'   data = test_data,
#'   expo_mean = tud_mean,
#'   expo_sd = tud_sd,
#'   seed = 837745
#' )
#'
#' ModF2 <- evaluate_clm(
#'   f2,
#'   data = test_data,
#'   expo_mean = tud_mean,
#'   expo_sd = tud_sd,
#'   seed = 834568
#' )
#'
#' compare_plot(ModF1, ModF2)
#'
compare_plot <- function(..., my_xlim = c(0, 2)) {
  # Check xlim
  checkmate::assert_numeric(
    my_xlim,
    any.missing = FALSE,
    len = 2,
    null.ok = FALSE
  )
  if (my_xlim[1] >= my_xlim[2]) {
    cli::cli_abort(
      c("x" = "The first value of `my_xlim` must be smaller than the second.")
    )
  }

  # Get and check input models
  m <- rlang::dots_list(..., .named = TRUE)
  m |>
    purrr::map(\(x) {
      checkmate::assert_list(
        x,
        types = c("list", "numeric"),
        any.missing = FALSE,
        null.ok = FALSE,
        names = "unique",
        len = 4
      )
      checkmate::assert_subset(
        names(x),
        choices = c("fit", "nobs", "coefs", "pred")
      )
    })
  if (length(m) > 5) {
    cli::cli_abort("Add more colors to `my_colors`.")
  }

  # Get colours
  my_colors <- c("black", "darkorange", "blue", "grey", "red")[seq_along(m)]

  # Get outcome
  my_outcome <- m |>
    purrr::map(\(x) {
      x$fit |>
        get_outcome() |>
        magrittr::extract2("type")
    }) |>
    unlist() |>
    unique()

  if (length(my_outcome) > 1) {
    cli::cli_abort("Only combine models with the same outcome.")
  }

  if (identical(my_outcome, "srh")) {
    my_title <- "Outcome: Self-rated health"
  } else if (identical(my_outcome, "swl")) {
    my_title <- "Outcome: Satisfaction with life"
  } else {
    my_title <- "Unknown outcome variable"
  }

  # Data processing and plotting
  plot_d <- m |>
    purrr::imap(\(x, idx) {
      x$coefs |>
        dplyr::mutate(
          model = idx
        )
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data[["coef.type"]] == "inter_expo") |>
    dplyr::mutate(
      term = .data[["term"]] |>
        as.factor() |>
        forcats::fct_rev(),
      model = factor(
        .data[["model"]],
        levels = names(m)
      )
    )

  lvlterm <- nlevels(plot_d$term)

  plot_d |>
    ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = seq(1.5, (lvlterm - 1 + 0.5), by = 1),
      linewidth = 0.2,
      color = "grey"
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        x = .data[["estimate"]],
        xmin = .data[["lwr"]],
        xmax = .data[["upr"]],
        y = .data[["term"]],
        color = .data[["model"]]
      ),
      position = ggplot2::position_dodge(width = 0.9),
      size = 0.2
    ) +
    ggplot2::coord_cartesian(xlim = my_xlim) +
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme_sub_axis_y(title = ggplot2::element_blank()) +
    ggplot2::theme_sub_legend(position = "bottom") +
    ggplot2::theme_sub_panel(grid.major.y = ggplot2::element_blank()) +
    ggplot2::labs(
      color = "Model",
      title = my_title,
      x = "Odds Ratio (95% confidence interval)"
    )
}

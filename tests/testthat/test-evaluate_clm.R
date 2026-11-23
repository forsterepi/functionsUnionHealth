test_that("evaluate_clm() works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  n_of_sample <- 3

  n <- create_newdata(
    fit = f,
    fixed_expo_perc = input_fixed_expo_perc,
    expo_mean = tud_mean,
    expo_sd = tud_sd
  )
  p <- get_predictions(fit = f, newdata = n)
  s <- draw_samples(
    fit = f,
    pred = p,
    n_sample = n_of_sample,
    seed = 797179
  )
  su <- summarise_samples(f, s)
  com <- combine_summaries(su, test_data)

  out <- evaluate_clm(
    fit = f,
    n_sample = n_of_sample,
    fixed_expo_perc = input_fixed_expo_perc,
    data = test_data,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    seed = 797179
  )

  expect_equal(out$pred, com)
  expect_equal(out$fit, f)
  expect_equal(out$nobs, nrow(f$model))
  expect_equal(
    out$coefs,
    coefs_clm(
      fit = f,
      expo_mean = tud_mean,
      expo_sd = tud_sd
    )
  )
})

test_that("evaluate_clm() works without predictions", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  n_of_sample <- 3

  out <- evaluate_clm(
    fit = f,
    n_sample = n_of_sample,
    fixed_expo_perc = input_fixed_expo_perc,
    data = test_data,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    seed = 797179,
    predict = FALSE
  )

  expect_equal(out$pred, NA)
  expect_equal(out$fit, f)
  expect_equal(out$nobs, nrow(f$model))
  expect_equal(
    out$coefs,
    coefs_clm(
      fit = f,
      expo_mean = tud_mean,
      expo_sd = tud_sd
    )
  )
})

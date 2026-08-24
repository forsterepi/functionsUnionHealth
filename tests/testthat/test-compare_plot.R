test_that("compare_plot() works", {
  f1 <- ordinal::clm(
    swl ~ cbc * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  f2 <- ordinal::clm(
    swl ~ cbc * decades_since_start + age_group + gender + job,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()

  ModF1 <- evaluate_clm(
    f1,
    data = test_data,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    seed = 837745
  )

  ModF2 <- evaluate_clm(
    f2,
    data = test_data,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    seed = 834568
  )

  expect_no_error(compare_plot(ModF1, ModF2))
})

test_that("coefs_clm() creates correct terms", {
  # TUD
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    fixed_year = 1990,
    fixed_expo_perc = 0.55
  )

  expect_equal(co$term, c("TUD (1990)", "Decade (TUD=55%)"))

  # CBC
  f <- ordinal::clm(
    swl ~ cbc * decades_since_start + tud,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  cbc_mean <- test_data$cbc_original |> mean()
  cbc_sd <- test_data$cbc_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = cbc_mean,
    expo_sd = cbc_sd,
    fixed_year = 1990,
    fixed_expo_perc = 0.55
  )

  expect_true(
    c("CBC (1990)", "Decade (CBC=55%)", "tud") |>
      magrittr::is_in(co$term) |>
      all()
  )
})

test_that("coefs_clm() gives the correct estimates for var = 0", {
  # TUD
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    fixed_year = 1981, # decades_since_start is 0 for 1981
    fixed_expo_perc = tud_mean # tud is 0 at its mean
  )

  co2 <- broom::tidy(f) |>
    dplyr::filter(.data[["coef.type"]] == "location") |>
    dplyr::mutate(
      lwr = estimate - 1.96 * std.error,
      upr = estimate + 1.96 * std.error,
      estimate = exp(estimate),
      lwr = exp(lwr),
      upr = exp(upr)
    )

  expect_equal(
    co$estimate[co$term == "TUD (1981)"],
    co2$estimate[co2$term == "tud"]
  )
  expect_equal(
    co$lwr[co$term == "TUD (1981)"],
    co2$lwr[co2$term == "tud"]
  )
  expect_equal(
    co$upr[co$term == "TUD (1981)"],
    co2$upr[co2$term == "tud"]
  )

  term_dec <- paste0("Decade (TUD=", round(tud_mean * 100, 2), "%)")

  expect_equal(
    co$estimate[co$term == term_dec],
    co2$estimate[co2$term == "decades_since_start"]
  )
  expect_equal(
    co$lwr[co$term == term_dec],
    co2$lwr[co2$term == "decades_since_start"]
  )
  expect_equal(
    co$upr[co$term == term_dec],
    co2$upr[co2$term == "decades_since_start"]
  )

  # CBC
  f <- ordinal::clm(
    swl ~ cbc * decades_since_start + tud,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  cbc_mean <- test_data$cbc_original |> mean()
  cbc_sd <- test_data$cbc_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = cbc_mean,
    expo_sd = cbc_sd,
    fixed_year = 1981, # decades_since_start is 0 for 1981
    fixed_expo_perc = cbc_mean # cbc is 0 at its mean
  )

  co2 <- broom::tidy(f) |>
    dplyr::filter(.data[["coef.type"]] == "location") |>
    dplyr::mutate(
      lwr = estimate - 1.96 * std.error,
      upr = estimate + 1.96 * std.error,
      estimate = exp(estimate),
      lwr = exp(lwr),
      upr = exp(upr)
    )

  expect_equal(
    co$estimate[co$term == "CBC (1981)"],
    co2$estimate[co2$term == "cbc"]
  )
  expect_equal(
    co$lwr[co$term == "CBC (1981)"],
    co2$lwr[co2$term == "cbc"]
  )
  expect_equal(
    co$upr[co$term == "CBC (1981)"],
    co2$upr[co2$term == "cbc"]
  )

  term_dec <- paste0("Decade (CBC=", round(cbc_mean * 100, 2), "%)")

  expect_equal(
    co$estimate[co$term == term_dec],
    co2$estimate[co2$term == "decades_since_start"]
  )
  expect_equal(
    co$lwr[co$term == term_dec],
    co2$lwr[co2$term == "decades_since_start"]
  )
  expect_equal(
    co$upr[co$term == term_dec],
    co2$upr[co2$term == "decades_since_start"]
  )
})

test_that("coefs_clm() correctly calculates interaction effects", {
  # TUD
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = tud_mean,
    expo_sd = tud_sd,
    fixed_year = 2004,
    fixed_expo_perc = 0.73
  )

  f_c_tud <- f$coefficients["tud"] |>
    magrittr::set_names(NULL)
  f_c_dec <- f$coefficients["decades_since_start"] |>
    magrittr::set_names(NULL)
  f_c_tud_dec <- f$coefficients["tud:decades_since_start"] |>
    magrittr::set_names(NULL)

  est_tud_2004 <- exp(
    f_c_tud +
      (2004 - 1981) / 10 * f_c_tud_dec
  )
  est_dec_73 <- exp(
    f_c_dec +
      (0.73 - tud_mean) / tud_sd * f_c_tud_dec
  )

  se_tud_2004 <- sqrt(
    stats::vcov(f)["tud", "tud"] +
      ((2004 - 1981) / 10)^2 *
        stats::vcov(f)["tud:decades_since_start", "tud:decades_since_start"] +
      2 *
        ((2004 - 1981) / 10) *
        stats::vcov(f)["tud", "tud:decades_since_start"]
  )

  se_dec_73 <- sqrt(
    stats::vcov(f)["decades_since_start", "decades_since_start"] +
      ((0.73 - tud_mean) / tud_sd)^2 *
        stats::vcov(f)["tud:decades_since_start", "tud:decades_since_start"] +
      2 *
        ((0.73 - tud_mean) / tud_sd) *
        stats::vcov(f)["decades_since_start", "tud:decades_since_start"]
  )

  lwr_tud_2004 <- exp(
    f_c_tud +
      (2004 - 1981) / 10 * f_c_tud_dec -
      1.96 * se_tud_2004
  )

  upr_tud_2004 <- exp(
    f_c_tud +
      (2004 - 1981) / 10 * f_c_tud_dec +
      1.96 * se_tud_2004
  )

  lwr_dec_73 <- exp(
    f_c_dec +
      (0.73 - tud_mean) / tud_sd * f_c_tud_dec -
      1.96 * se_dec_73
  )

  upr_dec_73 <- exp(
    f_c_dec +
      (0.73 - tud_mean) / tud_sd * f_c_tud_dec +
      1.96 * se_dec_73
  )

  expect_equal(co$estimate[co$term == "TUD (2004)"], est_tud_2004)
  expect_equal(co$estimate[co$term == "Decade (TUD=73%)"], est_dec_73)
  expect_equal(co$lwr[co$term == "TUD (2004)"], lwr_tud_2004)
  expect_equal(co$lwr[co$term == "Decade (TUD=73%)"], lwr_dec_73)
  expect_equal(co$upr[co$term == "TUD (2004)"], upr_tud_2004)
  expect_equal(co$upr[co$term == "Decade (TUD=73%)"], upr_dec_73)

  # CBC
  f <- ordinal::clm(
    swl ~ cbc * decades_since_start + tud,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  cbc_mean <- test_data$cbc_original |> mean()
  cbc_sd <- test_data$cbc_original |> sd()

  co <- coefs_clm(
    fit = f,
    expo_mean = cbc_mean,
    expo_sd = cbc_sd,
    fixed_year = 2013,
    fixed_expo_perc = 0.59
  )

  f_c_cbc <- f$coefficients["cbc"] |>
    magrittr::set_names(NULL)
  f_c_dec <- f$coefficients["decades_since_start"] |>
    magrittr::set_names(NULL)
  f_c_cbc_dec <- f$coefficients["cbc:decades_since_start"] |>
    magrittr::set_names(NULL)

  est_cbc_2013 <- exp(
    f_c_cbc +
      (2013 - 1981) / 10 * f_c_cbc_dec
  )
  est_dec_59 <- exp(
    f_c_dec +
      (0.59 - cbc_mean) / cbc_sd * f_c_cbc_dec
  )

  se_cbc_2013 <- sqrt(
    stats::vcov(f)["cbc", "cbc"] +
      ((2013 - 1981) / 10)^2 *
        stats::vcov(f)["cbc:decades_since_start", "cbc:decades_since_start"] +
      2 *
        ((2013 - 1981) / 10) *
        stats::vcov(f)["cbc", "cbc:decades_since_start"]
  )

  se_dec_59 <- sqrt(
    stats::vcov(f)["decades_since_start", "decades_since_start"] +
      ((0.59 - cbc_mean) / cbc_sd)^2 *
        stats::vcov(f)["cbc:decades_since_start", "cbc:decades_since_start"] +
      2 *
        ((0.59 - cbc_mean) / cbc_sd) *
        stats::vcov(f)["decades_since_start", "cbc:decades_since_start"]
  )

  lwr_cbc_2013 <- exp(
    f_c_cbc +
      (2013 - 1981) / 10 * f_c_cbc_dec -
      1.96 * se_cbc_2013
  )

  upr_cbc_2013 <- exp(
    f_c_cbc +
      (2013 - 1981) / 10 * f_c_cbc_dec +
      1.96 * se_cbc_2013
  )

  lwr_dec_59 <- exp(
    f_c_dec +
      (0.59 - cbc_mean) / cbc_sd * f_c_cbc_dec -
      1.96 * se_dec_59
  )

  upr_dec_59 <- exp(
    f_c_dec +
      (0.59 - cbc_mean) / cbc_sd * f_c_cbc_dec +
      1.96 * se_dec_59
  )

  expect_equal(co$estimate[co$term == "CBC (2013)"], est_cbc_2013)
  expect_equal(co$estimate[co$term == "Decade (CBC=59%)"], est_dec_59)
  expect_equal(co$lwr[co$term == "CBC (2013)"], lwr_cbc_2013)
  expect_equal(co$lwr[co$term == "Decade (CBC=59%)"], lwr_dec_59)
  expect_equal(co$upr[co$term == "CBC (2013)"], upr_cbc_2013)
  expect_equal(co$upr[co$term == "Decade (CBC=59%)"], upr_dec_59)
})

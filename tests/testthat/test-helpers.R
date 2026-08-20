test_that("get_exposure works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_equal(get_exposure(f), "tud")

  f <- ordinal::clm(
    swl ~ cbc * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_equal(get_exposure(f), "cbc")

  f <- ordinal::clm(
    swl ~ tud * decades_since_start + cbc,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_equal(get_exposure(f), "tud")

  f <- ordinal::clm(
    swl ~ cbc * decades_since_start + tud,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_equal(get_exposure(f), "cbc")

  f <- ordinal::clm(
    swl ~ cbc,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_error(get_exposure(f))

  f <- ordinal::clm(
    swl ~ tud,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )
  expect_error(get_exposure(f))
})

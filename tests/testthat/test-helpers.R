test_that("get_exposure() works", {
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

  f <- ordinal::clm(
    swl ~ tud,
    data = test_data,
    link = "logit"
  )
  expect_error(get_exposure(f), class = "error.cutpoints.by.country")
})

test_that("get_outcome() works", {
  f <- ordinal::clm(
    swl ~ tud,
    data = test_data,
    link = "logit"
  )
  expect_error(get_outcome(f), class = "error.cutpoints.by.country")

  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  expect_equal(
    get_outcome(f),
    list(
      type = "swl",
      int = 1:5,
      colnames = c("very.high", "high", "medium", "low", "very.low")
    )
  )

  f <- ordinal::clm(
    srh ~ tud * decades_since_start + cbc,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  expect_equal(
    get_outcome(f),
    list(
      type = "srh",
      int = 1:4,
      colnames = c("very.good", "good", "fair", "poor.or.very.poor")
    )
  )

  d_test <- test_data |>
    dplyr::mutate(
      job2 = job |> factor(ordered = TRUE)
    )

  f <- ordinal::clm(
    job2 ~ cbc * decades_since_start,
    nominal = ~country,
    data = d_test,
    link = "logit"
  )

  expect_error(get_outcome(f))
})

test_that("create_newdata() works", {
  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)

  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    data = test_data,
    link = "logit"
  )

  expect_error(
    create_newdata(
      fit = f,
      fixed_expo_perc = input_fixed_expo_perc,
      expo_mean = tud_mean,
      expo_sd = tud_sd
    ),
    class = "error.cutpoints.by.country"
  )

  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  n <- create_newdata(
    fit = f,
    fixed_expo_perc = input_fixed_expo_perc,
    expo_mean = tud_mean,
    expo_sd = tud_sd
  )

  expect_equal(length(n), nlevels(f$model$country))

  expect_equal(
    n |> purrr::map_int(length),
    rep(length(input_fixed_expo_perc), nlevels(f$model$country))
  )

  expect_true(
    n |>
      purrr::map(\(x) {
        x |>
          purrr::map(colnames)
      }) |>
      purrr::flatten() |>
      purrr::map_lgl(identical, colnames(n[[1]][[1]])) |>
      all()
  )

  first_country <- f$model$country |>
    unique() |>
    as.character() |>
    magrittr::extract(1)

  test_n <- f$model |>
    dplyr::filter(country == first_country) |>
    dplyr::mutate(
      tud = NULL,
      swl = NULL
    ) |>
    dplyr::mutate(
      tud = (input_fixed_expo_perc[1] - tud_mean) / tud_sd,
    )

  expect_equal(n[[1]][[1]], test_n)

  fifth_country <- f$model$country |>
    unique() |>
    as.character() |>
    magrittr::extract(5)

  test_n <- f$model |>
    dplyr::filter(country == fifth_country) |>
    dplyr::mutate(
      tud = NULL,
      swl = NULL
    ) |>
    dplyr::mutate(
      tud = (input_fixed_expo_perc[2] - tud_mean) / tud_sd,
    )

  expect_equal(n[[5]][[2]], test_n)
})

test_that("get_predictions() works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  outc <- get_outcome(f)

  n <- create_newdata(
    fit = f,
    fixed_expo_perc = input_fixed_expo_perc,
    expo_mean = tud_mean,
    expo_sd = tud_sd
  )

  pred <- get_predictions(fit = f, newdata = n)

  expect_equal(
    pred |>
      purrr::map_int(length),
    n |>
      purrr::map_int(length)
  )

  expect_true(
    pred |>
      purrr::map(\(x) {
        x |>
          purrr::map(colnames)
      }) |>
      purrr::flatten() |>
      purrr::map_lgl(identical, c(outc$colnames, colnames(n[[1]][[1]]))) |>
      all()
  )

  p <- predict(f, newdata = n[[1]][[1]])$fit
  colnames(p) <- p |>
    colnames() |>
    make.names()
  p <- cbind(p, n[[1]][[1]])

  expect_equal(pred[[1]][[1]], p)

  p <- predict(f, newdata = n[[length(n)]][[length(input_fixed_expo_perc)]])$fit
  colnames(p) <- p |>
    colnames() |>
    make.names()
  p <- cbind(p, n[[length(n)]][[length(input_fixed_expo_perc)]])

  expect_equal(pred[[length(n)]][[length(input_fixed_expo_perc)]], p)
})

test_that("draw_samples() works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  outc <- get_outcome(f)
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

  expect_equal(
    s |>
      purrr::map_int(length),
    p |>
      purrr::map_int(length)
  )

  expect_true(
    s |>
      purrr::map(\(x) {
        x |>
          purrr::map(colnames)
      }) |>
      purrr::flatten() |>
      purrr::map_lgl(
        identical,
        c(colnames(p[[1]][[1]]), paste0("draw", 1:n_of_sample))
      ) |>
      all()
  )

  expect_true(
    s |>
      purrr::map(\(x) {
        x |>
          purrr::map(\(y) {
            y |>
              dplyr::select(dplyr::starts_with("draw")) |>
              purrr::map(unique)
          })
      }) |>
      purrr::flatten() |>
      purrr::flatten() |>
      purrr::map(sort) |>
      purrr::map_lgl(identical, outc$int) |>
      all()
  )

  p2 <- p[1]
  p2[[1]][[2]] <- NULL

  n_test <- 1e4

  s2 <- draw_samples(
    fit = f,
    pred = p2,
    n_sample = n_test,
    seed = 413197
  )

  row <- 1
  plus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::add(0.01)
  minus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::subtract(0.01)

  expect_true(
    s2[[1]][[1]][row, ] |>
      dplyr::select(dplyr::starts_with("draw")) |>
      unlist() |>
      plyr::count() |>
      dplyr::mutate(
        prop = round(freq / n_test, 2)
      ) |>
      magrittr::use_series("prop") |>
      purrr::imap_lgl(\(x, idx) {
        dplyr::between(x, minus1[idx], plus1[idx])
      }) |>
      all()
  )

  row <- 54
  plus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::add(0.01)
  minus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::subtract(0.01)

  expect_true(
    s2[[1]][[1]][row, ] |>
      dplyr::select(dplyr::starts_with("draw")) |>
      unlist() |>
      plyr::count() |>
      dplyr::mutate(
        prop = round(freq / n_test, 2)
      ) |>
      magrittr::use_series("prop") |>
      purrr::imap_lgl(\(x, idx) {
        dplyr::between(x, minus1[idx], plus1[idx])
      }) |>
      all()
  )

  row <- 199
  plus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::add(0.01)
  minus1 <- s2[[1]][[1]][row, outc$colnames] |>
    unlist() |>
    round(2) |>
    magrittr::set_names(NULL) |>
    magrittr::subtract(0.01)

  expect_true(
    s2[[1]][[1]][row, ] |>
      dplyr::select(dplyr::starts_with("draw")) |>
      unlist() |>
      plyr::count() |>
      dplyr::mutate(
        prop = round(freq / n_test, 2)
      ) |>
      magrittr::use_series("prop") |>
      purrr::imap_lgl(\(x, idx) {
        dplyr::between(x, minus1[idx], plus1[idx])
      }) |>
      all()
  )
})

test_that("summarise_samples() works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  outc <- get_outcome(f)
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

  expect_equal(
    su |>
      purrr::map_int(length),
    s |>
      purrr::map_int(length)
  )

  expect_equal(
    s |>
      purrr::map_int(\(x) {
        x[[1]] |>
          magrittr::use_series("decades_since_start") |>
          unique() |>
          length()
      }),
    su |>
      purrr::map(\(x) {
        x |>
          purrr::map_int(nrow) |>
          unique()
      }) |>
      unlist()
  )

  t <- s[[1]][[1]]

  t19 <- t |>
    dplyr::filter(decades_since_start == 1.9)
  t25 <- t |>
    dplyr::filter(decades_since_start == 2.5)
  cou <- t$country |>
    unique()
  tud <- t$tud |>
    unique()

  means19 <- c(
    mean(t19$draw1),
    mean(t19$draw2),
    mean(t19$draw3)
  )
  means25 <- c(
    mean(t25$draw1),
    mean(t25$draw2),
    mean(t25$draw3)
  )

  mean_means19 <- mean(means19)
  q025_means19 <- quantile(means19, probs = 0.025)
  q975_means19 <- quantile(means19, probs = 0.975)

  mean_means25 <- mean(means25)
  q025_means25 <- quantile(means25, probs = 0.025)
  q975_means25 <- quantile(means25, probs = 0.975)

  out <- data.frame(
    decades_since_start = c(1.9, 2.5),
    mean_sampled = c(mean_means19, mean_means25),
    q025_sampled = c(q025_means19, q025_means25),
    q975_sampled = c(q975_means19, q975_means25),
    country = c(cou, cou),
    tud = c(tud, tud)
  ) |>
    tibble::as_tibble()

  expect_equal(su[[1]][[1]], out)
})

test_that("combine_summaries() works", {
  f <- ordinal::clm(
    swl ~ tud * decades_since_start,
    nominal = ~country,
    data = test_data,
    link = "logit"
  )

  tud_mean <- test_data$tud_original |> mean()
  tud_sd <- test_data$tud_original |> sd()
  input_fixed_expo_perc <- c(0.1, 0.5)
  outc <- get_outcome(f)
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

  expect_equal(
    nrow(com),
    su |>
      purrr::map(\(x) {
        x |>
          purrr::map_int(nrow)
      }) |>
      unlist() |>
      sum()
  )

  add_can <- data.frame(
    year = c(2000, 2006),
    class = c("high", "high"),
    c = c("CAN", "CAN"),
    wave = c(4, 5)
  )

  add_col <- data.frame(
    c = c("COL", "COL"),
    year = c(1998, 2018),
    class = c("low", "low"),
    wave = c(3, 7)
  )

  add_egy <- data.frame(
    c = c("EGY", "EGY"),
    year = c(2008, 2018),
    class = c("low", "lower-middle"),
    wave = c(5, 7)
  )

  add_svn <- data.frame(
    c = c("SVN", "SVN"),
    year = c(1995, 2005),
    class = c("upper-middle", "high"),
    wave = c(3, 5)
  )

  add_ury <- data.frame(
    c = c("URY", "URY"),
    year = c(1996, 2006),
    class = c("lower-middle", "upper-middle"),
    wave = c(3, 5)
  )

  out <- vctrs::vec_rbind(
    vctrs::vec_cbind(
      su[[1]][[1]],
      add_can
    ),
    vctrs::vec_cbind(
      su[[1]][[2]],
      add_can
    ),
    vctrs::vec_cbind(
      su[[2]][[1]],
      add_col
    ),
    vctrs::vec_cbind(
      su[[2]][[2]],
      add_col
    ),
    vctrs::vec_cbind(
      su[[3]][[1]],
      add_egy
    ),
    vctrs::vec_cbind(
      su[[3]][[2]],
      add_egy
    ),
    vctrs::vec_cbind(
      su[[4]][[1]],
      add_svn
    ),
    vctrs::vec_cbind(
      su[[4]][[2]],
      add_svn
    ),
    vctrs::vec_cbind(
      su[[5]][[1]],
      add_ury
    ),
    vctrs::vec_cbind(
      su[[5]][[2]],
      add_ury
    )
  )

  out$year <- as.factor(out$year)
  out$class <- as.factor(out$class)
  out$wave <- as.factor(out$wave)

  expect_equal(com, out)
})

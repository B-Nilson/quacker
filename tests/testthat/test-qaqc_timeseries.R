test_that("basic case works", {
  example_ts <- data.frame(
    date = seq(
      lubridate::ymd_h("2022-01-01 00"),
      lubridate::ymd_h("2022-01-01 23"),
      by = "1 hour"
    ),
    value_a = 1:24,
    value_b = rep(1:6, each = 3) |> c(100, 0, 0, 0, 0, NA)
  )
  example_ts <- example_ts[-13, ] # drop row 13

  qaqc_timeseries(
    ts_data = example_ts,
    date_col = "date",
    value_cols = c("value_a", "value_b"),
    allowed_range = c(0, 80),
    allowed_steps = list("1 hours" = 5, "3 hours" = 100),
    allowed_repeats = 3
  ) |>
    print(n = 24) |>
    expect_snapshot()
})

test_that("assessing repeats works", {
  test_values <- 1:10
  for_repeats <- 1:5

  repeated_values <- for_repeats |>
    lapply(\(reps) rep(test_values, each = reps))

  for (n_repeats in for_repeats) {
    values <- repeated_values[[n_repeats]]
    # Expect all repeats if threshold just below # of repeats (except the non-repeating case)
    if (n_repeats != 1) {
      expect_true(all(assess_repeating(values, max_repeats = n_repeats - 1)))
    }
    # Expect no repeats if threshold at # of repeats
    expect_true(!any(assess_repeating(values, max_repeats = n_repeats)))
  }
})

test_that("assessing spikes works", {
  test_values <- 1:10 |> c(12, 14:20, 100, 0, 0, 0, 0)
  time_step <- "1 hours"
  max_steps <- list("1 hours" = 2, "3 hours" = 50)

  is_spiking <- test_values |>
    assess_spiking(max_steps = max_steps, time_step = time_step)

  # spikes are 12, 14, 100, and all but the last 0
  expect_true(identical(
    c(which(is_spiking), length(test_values)), # pretend last 0 is a spike
    which(test_values %in% c(12, 14, 100, 0))
  ))
})

test_that("assessing range works", {
  test_values <- 1:10
  expect_true(!any(assess_range(test_values, range = range(test_values))))
  expect_true(all(assess_range(
    test_values,
    range = range(test_values) - max(test_values)
  )))
  expect_true(all(assess_range(
    test_values,
    range = range(test_values) + max(test_values)
  )))
})

test_that("assessing missingness works", {
  test_values <- c(NA, NA, 1:10)
  is_missing <- assess_missing(test_values)
  expect_true(!any(is_missing[-(1:2)]))
  expect_true(all(is_missing[1:2]))
  expect_true(!all(is_missing))
})

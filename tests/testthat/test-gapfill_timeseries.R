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

  with_gap <- example_ts[-13, ] # drop row 13

  gapfilled <- gapfill_timeseries(
    ts_data = with_gap,
    date_col = "date"
  )

  expect_identical(names(example_ts), names(gapfilled))
  expect_identical(example_ts$date, gapfilled$date)
  expect_identical(example_ts$value_a[-13], gapfilled$value_a[-13])
  expect_true(all(is.na(unlist(gapfilled[13, -1]))))
})

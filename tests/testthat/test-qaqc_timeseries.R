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

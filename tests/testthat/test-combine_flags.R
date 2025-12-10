test_that("basic case works", {
  example_ts <- data.frame(
    date = seq(
      lubridate::ymd_h("2022-01-01 00"),
      lubridate::ymd_h("2022-01-01 23"),
      by = "1 hour"
    ),
    value_a = 1:24,
    value_b = rep(1:6, each = 3) |> c(100, 0, 0, 0, 0, NA)
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("value_a", "value_b")),
        list(
          is_missing = \(x) assess_missing(x),
          is_out_of_range = \(x) assess_range(x, range = c(0, 80))
        ),
        .names = ".flag_{.col}_{.fn}"
      )
    )

  example_ts |>
    combine_flags(
      value_cols = c("value_a", "value_b"),
      flag_prefix = ".flag_",
      list_prefix = ".flag_list_",
      name_prefix = ".flag_name_"
    ) |>
    print(n = 24) |>
    expect_snapshot()

  # TODO: more exact test
})

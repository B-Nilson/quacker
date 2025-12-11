
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quacker

<!-- badges: start -->

<!-- badges: end -->

The goal of quacker is to improve the ease and consistency of quality
analysis and quality control (QA/QC) methods used in data science.

quacker is still in active development.

## Installation

You can install the development version of quacker like so:

``` r
# install.packages("pak")
pak::pak("B-Nilson/quacker")
```

## Example

Here is how you can use quaker to QA/QC timeseries data:

``` r
library(quacker)

example_ts <- data.frame(
  date = seq(
    lubridate::ymd_h("2022-01-01 00"),
    lubridate::ymd_h("2022-01-01 23"),
    by = "1 hour"
  ),
  value_a = 1:24,
  value_b = rep(1:6, each = 3) |> c(100, 0, 0, 0, 0, NA)
)

flagged_data <- qaqc_timeseries(
  ts_data = example_ts,
  date_col = "date",
  value_cols = c("value_a", "value_b"),
  allowed_range = c(0, 80),
  allowed_steps = list("1 hours" = 5, "3 hours" = 100),
  allowed_repeats = 3,
  precision = 1
)

flagged_data |>
  dplyr::select(-"date") |> # for cleaner printing
  print(n = 24)
#> # A tibble: 24 × 8
#>    value_a value_b .flag_value_a .flags_value_a .flag_name_value_a .flag_value_b
#>      <int>   <dbl>         <int> <list>         <chr>                      <int>
#>  1       1       1             0 <tibble>       <NA>                           0
#>  2       2       1             0 <tibble>       <NA>                           0
#>  3       3       1             0 <tibble>       <NA>                           0
#>  4       4       2             0 <tibble>       <NA>                           0
#>  5       5       2             0 <tibble>       <NA>                           0
#>  6       6       2             0 <tibble>       <NA>                           0
#>  7       7       3             0 <tibble>       <NA>                           0
#>  8       8       3             0 <tibble>       <NA>                           0
#>  9       9       3             0 <tibble>       <NA>                           0
#> 10      10       4             0 <tibble>       <NA>                           0
#> 11      11       4             0 <tibble>       <NA>                           0
#> 12      12       4             0 <tibble>       <NA>                           0
#> 13      13       5             0 <tibble>       <NA>                           0
#> 14      14       5             0 <tibble>       <NA>                           0
#> 15      15       5             0 <tibble>       <NA>                           0
#> 16      16       6             0 <tibble>       <NA>                           0
#> 17      17       6             0 <tibble>       <NA>                           0
#> 18      18       6             0 <tibble>       <NA>                           0
#> 19      19     100             0 <tibble>       <NA>                          12
#> 20      20       0             0 <tibble>       <NA>                           7
#> 21      21       0             0 <tibble>       <NA>                           7
#> 22      22       0             0 <tibble>       <NA>                           7
#> 23      23       0             0 <tibble>       <NA>                           3
#> 24      24      NA             0 <tibble>       <NA>                          16
#> # ℹ 2 more variables: .flags_value_b <list>, .flag_name_value_b <chr>

flagged_data |> 
  plot_timeseries_qaqc()
#> Warning in guess_date_col(date_col, data = qaqced_ts_data): guessing `date_col`
#> from provided data - set `date_col` directly to quiet this warning
#> Warning in guess_value_cols(value_cols, data = qaqced_ts_data): guessing
#> `value_cols` from provided data - set `value_cols` directly to quiet this
#> warning
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the quacker package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 7 rows containing non-finite outside the scale range
#> (`stat_density()`).
```

<img src="man/figures/README-example-1.png" width="100%" />

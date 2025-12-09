# basic case works

    Code
      print(qaqc_timeseries(ts_data = example_ts, date_col = "date", value_cols = c(
        "value_a", "value_b"), allowed_range = c(0, 80), allowed_steps = list(
        `1 hours` = 5, `3 hours` = 100), allowed_repeats = 3), n = 24)
    Output
      # A tibble: 23 x 7
         date                value_a value_b .flag_value_a .flags_value_a  
         <dttm>                <int>   <dbl>         <int> <list>          
       1 2022-01-01 00:00:00       1       1             0 <tibble [1 x 5]>
       2 2022-01-01 01:00:00       2       1             0 <tibble [1 x 5]>
       3 2022-01-01 02:00:00       3       1             0 <tibble [1 x 5]>
       4 2022-01-01 03:00:00       4       2             0 <tibble [1 x 5]>
       5 2022-01-01 04:00:00       5       2             0 <tibble [1 x 5]>
       6 2022-01-01 05:00:00       6       2             0 <tibble [1 x 5]>
       7 2022-01-01 06:00:00       7       3             0 <tibble [1 x 5]>
       8 2022-01-01 07:00:00       8       3             0 <tibble [1 x 5]>
       9 2022-01-01 08:00:00       9       3             0 <tibble [1 x 5]>
      10 2022-01-01 09:00:00      10       4             0 <tibble [1 x 5]>
      11 2022-01-01 10:00:00      11       4             0 <tibble [1 x 5]>
      12 2022-01-01 11:00:00      12       4             0 <tibble [1 x 5]>
      13 2022-01-01 13:00:00      14       5             0 <tibble [1 x 5]>
      14 2022-01-01 14:00:00      15       5             0 <tibble [1 x 5]>
      15 2022-01-01 15:00:00      16       6             0 <tibble [1 x 5]>
      16 2022-01-01 16:00:00      17       6             0 <tibble [1 x 5]>
      17 2022-01-01 17:00:00      18       6             0 <tibble [1 x 5]>
      18 2022-01-01 18:00:00      19     100             0 <tibble [1 x 5]>
      19 2022-01-01 19:00:00      20       0             0 <tibble [1 x 5]>
      20 2022-01-01 20:00:00      21       0             0 <tibble [1 x 5]>
      21 2022-01-01 21:00:00      22       0             0 <tibble [1 x 5]>
      22 2022-01-01 22:00:00      23       0             0 <tibble [1 x 5]>
      23 2022-01-01 23:00:00      24      NA             0 <tibble [1 x 5]>
      # i 2 more variables: .flag_value_b <int>, .flags_value_b <list>


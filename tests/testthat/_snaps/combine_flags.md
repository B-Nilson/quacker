# basic case works

    Code
      print(combine_flags(example_ts, value_cols = c("value_a", "value_b"),
      flag_prefix = ".flag_", list_prefix = ".flag_list_", name_prefix = ".flag_name_"),
      n = 24)
    Output
      # A tibble: 24 x 9
         date                value_a value_b .flag_value_a .flag_list_value_a
         <dttm>                <int>   <dbl>         <int> <list>            
       1 2022-01-01 00:00:00       1       1             0 <tibble [1 x 2]>  
       2 2022-01-01 01:00:00       2       1             0 <tibble [1 x 2]>  
       3 2022-01-01 02:00:00       3       1             0 <tibble [1 x 2]>  
       4 2022-01-01 03:00:00       4       2             0 <tibble [1 x 2]>  
       5 2022-01-01 04:00:00       5       2             0 <tibble [1 x 2]>  
       6 2022-01-01 05:00:00       6       2             0 <tibble [1 x 2]>  
       7 2022-01-01 06:00:00       7       3             0 <tibble [1 x 2]>  
       8 2022-01-01 07:00:00       8       3             0 <tibble [1 x 2]>  
       9 2022-01-01 08:00:00       9       3             0 <tibble [1 x 2]>  
      10 2022-01-01 09:00:00      10       4             0 <tibble [1 x 2]>  
      11 2022-01-01 10:00:00      11       4             0 <tibble [1 x 2]>  
      12 2022-01-01 11:00:00      12       4             0 <tibble [1 x 2]>  
      13 2022-01-01 12:00:00      13       5             0 <tibble [1 x 2]>  
      14 2022-01-01 13:00:00      14       5             0 <tibble [1 x 2]>  
      15 2022-01-01 14:00:00      15       5             0 <tibble [1 x 2]>  
      16 2022-01-01 15:00:00      16       6             0 <tibble [1 x 2]>  
      17 2022-01-01 16:00:00      17       6             0 <tibble [1 x 2]>  
      18 2022-01-01 17:00:00      18       6             0 <tibble [1 x 2]>  
      19 2022-01-01 18:00:00      19     100             0 <tibble [1 x 2]>  
      20 2022-01-01 19:00:00      20       0             0 <tibble [1 x 2]>  
      21 2022-01-01 20:00:00      21       0             0 <tibble [1 x 2]>  
      22 2022-01-01 21:00:00      22       0             0 <tibble [1 x 2]>  
      23 2022-01-01 22:00:00      23       0             0 <tibble [1 x 2]>  
      24 2022-01-01 23:00:00      24      NA             0 <tibble [1 x 2]>  
      # i 4 more variables: .flag_name_value_a <chr>, .flag_value_b <int>,
      #   .flag_list_value_b <list>, .flag_name_value_b <chr>


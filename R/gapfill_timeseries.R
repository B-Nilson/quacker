gapfill_timeseries <- function(ts_data, date_col, time_step = NA, sort = TRUE) {
  stopifnot(is.data.frame(ts_data))
  stopifnot(
    is.na(time_step) | !is.na(lubridate::as.period(time_step)),
    length(time_step) == 1
  )

  # handle tidyselect for date_col
  date_col <- date_col |> tidyselect::eval_select(data = ts_data)
  date_col_str <- names(date_col)
  single_date_col <- length(date_col) == 1
  stopifnot(
    "`date_col` must resolve to a single column in `ts_data` using tidyselect (ex. date_utc, 'date_utc' or dplyr::starts_with('date'))" = single_date_col
  )

  # determine time_step from ts_data if not provided
  if (is.na(time_step)) {
    time_step <- handyr::get_step(ts_data[[date_col]])
  }

  # Fill in missing dates
  ts_data <- ts_data |>
    tidyr::complete(
      {{ date_col_str }} := seq(
        min(.data[[date_col_str]], na.rm = TRUE),
        max(.data[[date_col_str]], na.rm = TRUE),
        by = time_step
      )
    )
  
  if (sort) {
    ts_data <- ts_data |>
      dplyr::arrange(.data[[date_col_str]])
  }
  return(ts_data)
}

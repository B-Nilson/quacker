gapfill_timeseries <- function(
  ts_data,
  date_col = NULL,
  time_step = NA,
  sort = TRUE
) {
  stopifnot(is.data.frame(ts_data))
  stopifnot(
    is.na(time_step) | !is.na(lubridate::as.period(time_step)),
    length(time_step) == 1
  )

  # handle tidyselect for date_col
  date_col <- date_col |>
    guess_date_col(date_col = date_col) |>
    handle_tidyselect(data = ts_data, .expect_one = TRUE)

  # determine time_step from ts_data if not provided
  if (is.na(time_step)) {
    time_step <- handyr::get_step(ts_data[[date_col]])
  }

  # Fill in missing dates
  date_col_str <- names(date_col)
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

# TODO: move to handyr?
handle_tidyselect <- function(
  data,
  ...,
  .expect_one = FALSE,
  .expect_some = FALSE
) {
  cols <- tidyselect::eval_select(..., data = data)
  if (.expect_some) {
    some_cols <- length(cols) > 0
    stopifnot(
      "tidyselect must resolve to one or more columns in provided data" = some_cols
    )
  }
  if (.expect_one) {
    one_col <- length(cols) == 1
    stopifnot(
      "tidyselect must resolve to a single column in provided data" = one_col
    )
  }
  return(cols)
}

guess_date_col <- function(data, date_col = NULL) {
  if (rlang::is_null(date_col)) {
    warning(
      "guessing `date_col` from provided data - set `date_col` directly to quiet this warning"
    )
    date_col <- data |>
      sapply(\(x) lubridate::is.Date(x) | lubridate::is.POSIXct(x)) |>
      which()
    if (length(date_col) == 0) {
      stop(
        "if `date_col` is not provided, data must contain at least one column with dates (the first column will be used)"
      )
    }
    date_col <- date_col[1]
  }
  return(date_col)
}

guess_value_cols <- function(data, value_cols = NULL, exclude_dot_cols = TRUE) {
  if (rlang::is_null(value_cols)) {
    warning(
      "guessing `value_cols` from provided data - set `value_cols` directly to quiet this warning"
    )
    value_cols <- dplyr::where(is.numeric) |>
      tidyselect::eval_select(data = data)
    if (exclude_dot_cols) {
      value_cols <- value_cols[!startsWith(names(value_cols), ".")]
    }
    if (length(value_cols) < 1) {
      stop(
        "if `value_cols` is not provided, provided data must contain at least one numeric column" |>
          paste(ifelse(
            exclude_dot_cols,
            " whose name does not start with `.`.",
            "."
          ))
      )
    }
  }
  return(value_cols)
}

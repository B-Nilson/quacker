# TODO: make rolling averages?
# TODO: allow different ranges / repeats for different columns
# TODO: flag_bad_sensor tried to allow for persitently low repeating values, not sure if that's useful
qaqc_timeseries <- function(
  ts_data,
  date_col,
  value_cols,
  time_step = NA,
  allowed_range = c(-Inf, Inf),
  allowed_steps = list("1 hours" = Inf),
  allowed_repeats = 3
) {
  stopifnot(is.data.frame(ts_data))
  stopifnot(
    is.na(time_step) | !is.na(lubridate::as.period(time_step)),
    length(time_step) == 1
  )
  # date_col & value_cols validated later

  # handle tidyselect for date_col and value_cols
  date_col <- date_col |> tidyselect::eval_select(data = ts_data)
  date_col_str <- names(date_col)
  value_cols <- value_cols |> tidyselect::eval_select(data = ts_data)

  # validate date_col/value_cols
  single_date_col <- length(date_col) == 1
  multiple_value_cols <- length(date_col) > 0
  stopifnot(
    "`date_col` must resolve to a single column in `ts_data` using tidyselect (ex. date_utc, 'date_utc' or dplyr::starts_with('date'))" = single_date_col
  )
  stopifnot(
    "`value_cols` must resolve to one or more columns in `ts_data` using tidyselect (ex. c(value_a, value_b), c('value_a', 'value_b') or dplyr::starts_with('value_'))" = multiple_value_cols
  )

  # determine time_step from ts_data if not provided
  if (is.na(time_step)) {
    time_step <- handyr::get_step(ts_data[[date_col]])
  }

  # fill time gaps, marking added rows - abstract this to a separate function?
  data_for_qc <- ts_data |>
    dplyr::mutate(.original_order = dplyr::row_number()) |>
    tidyr::complete(
      {{ date_col_str }} := seq(
        min(.data[[date_col_str]], na.rm = TRUE),
        max(.data[[date_col_str]], na.rm = TRUE),
        by = time_step
      )
    ) |>
    dplyr::arrange(.data[[date_col_str]])

  # define assessments
  assessments <- list(
    # assess missingness
    is_missing = is.na,
    # assess out-of-range-ness
    is_out_of_range = \(x) {
      result <- rep(FALSE, length(x))
      result[x < min(allowed_range, na.rm = TRUE)] <- TRUE
      result[x > max(allowed_range, na.rm = TRUE)] <- TRUE
      return(result)
    },
    # assess repeatedness
    is_repeating = \(x) x |> assess_repeating(max_repeats = allowed_repeats),
    is_repeating_at_range = \(x) {
      is_repeating <- x |> assess_repeating(max_repeats = allowed_repeats)
      is_repeating[!x %in% allowed_range] <- FALSE
      return(is_repeating)
    },
    # assess spikes
    is_spiking = \(x) {
      x |> assess_spiking(max_steps = allowed_steps, time_step = time_step)
    }
  )

  # apply assessments to each value column
  flagged <- data_for_qc |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(value_cols),
        assessments,
        .names = ".flag_{.col}_{.fn}"
      )
    )

  # build combined binary flag column for each value
  # and move seperated flags to a list column
  for (col in names(value_cols)) {
    combined_col <- paste0(".flag_", col)
    list_col <- paste0(".flags_", col)
    flags <- dplyr::starts_with(combined_col) |>
      tidyselect::eval_select(data = flagged)
    flagged <- flagged |>
      dplyr::mutate(
        !!combined_col := rowSums(
          dplyr::across(dplyr::all_of(flags), \(x) {
            flag_number <- which(names(flags) == dplyr::cur_column())
            as.integer(x) * 2^(flag_number - 1)
          }),
          na.rm = TRUE
        ),
        !!combined_col := as.integer(.data[[combined_col]])
      ) |>
      tidyr::nest(!!list_col := dplyr::any_of(names(flags)))
  }

  # revert gap-filling and sorting
  flagged |>
    dplyr::filter(!is.na(.data$.original_order)) |>
    dplyr::arrange(.data$.original_order) |>
    dplyr::select(-.original_order)
}

assess_repeating <- function(x, max_repeats = 3) {
  default <- min(x, na.rm = TRUE) - 1
  x[is.na(x)] <- default
  is_repeating <- x == dplyr::lag(x, default = default)
  if (max_repeats > 1) {
    for (lag_n in 2:max_repeats) {
      lagged <- dplyr::lag(x, n = lag_n, default = default)
      is_repeating <- is_repeating & x == lagged
    }
  }
  if (any(is_repeating)) {
    repeats <- lapply(1:max_repeats, \(n) which(is_repeating) - n) |>
      unlist() |>
      unique()
    repeats <- repeats[repeats > 0]
    is_repeating[repeats] <- TRUE
  }
  return(is_repeating)
}

assess_spiking <- function(x, max_steps = list("1 hours" = Inf), time_step) {
  time_step <- lubridate::as.period(time_step)
  thresholds <- lubridate::as.period(names(max_steps))
  is_spiking <- rep(FALSE, length(x))
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[[i]]
    lag_n <- threshold / time_step
    if (as.integer(lag_n) != lag_n) {
      stop(
        "thresholds (names of `max_steps`) must be a larger multiple of `time_step`"
      )
    }
    for (n in 1:lag_n) {
      is_spiking <- is_spiking |
        (abs(x - dplyr::lag(x, n = n)) >= max_steps[[i]]) |>
          handyr::swap(what = NA, with = FALSE)
    }
  }
  return(is_spiking)
}

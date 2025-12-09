# TODO: make rolling averages?
# TODO: allow different ranges / repeats for different columns
# TODO: flag_bad_sensor tried to allow for persitently low repeating values, not sure if that's useful
#' @importFrom rlang .data `:=`
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
  stopifnot(is.numeric(allowed_range), length(allowed_range) == 2)
  stopifnot(
    identical(class(allowed_steps), "list"),
    length(allowed_steps) > 0,
    all(lengths(allowed_steps) == 1),
    !anyNA(lubridate::as.period(names(allowed_steps)))
  )
  stopifnot(is.numeric(allowed_repeats), length(allowed_repeats) == 1)
  # date_col & value_cols validated later

  # handle tidyselect for value_cols
  value_cols <- value_cols |> tidyselect::eval_select(data = ts_data)
  multiple_value_cols <- length(value_cols) > 0
  stopifnot(
    "`value_cols` must resolve to one or more columns in `ts_data` using tidyselect (ex. c(value_a, value_b), c('value_a', 'value_b') or dplyr::starts_with('value_'))" = multiple_value_cols
  )

  # determine time_step from ts_data if not provided
  if (is.na(time_step)) {
    time_step <- handyr::get_step(ts_data[[date_col]])
  }

  # define assessments
  assessments <- list(
    # assess missingness
    is_missing = assess_missing,
    # assess out-of-range-ness
    is_out_of_range = \(x) x |> assess_range(range = allowed_range),
    # assess spikes
    is_spiking = \(x) {
      x |> assess_spiking(max_steps = allowed_steps, time_step = time_step)
    },
    # assess repeatedness
    is_repeating = \(x) x |> assess_repeating(max_repeats = allowed_repeats),
    is_repeating_at_range = \(x) {
      is_repeating <- x |> assess_repeating(max_repeats = allowed_repeats)
      is_repeating[!x %in% allowed_range] <- FALSE
      return(is_repeating)
    }
  )

  ts_data |>
    # fill time gaps, marking added rows
    dplyr::mutate(.original_order = dplyr::row_number()) |>
    gapfill_timeseries(date_col = date_col, time_step = time_step) |>
    # apply assessments to each value column
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(value_cols),
        assessments,
        .names = ".flag_{.col}_{.fn}"
      )
    ) |>
    # build combined binary flag column for each value
    # and move seperated flags to a list column
    combine_flags(
      value_cols = value_cols,
      flag_prefix = ".flag_",
      list_prefix = ".flags_"
    ) |>
    # revert gap-filling and sorting
    dplyr::filter(!is.na(.data$.original_order)) |>
    dplyr::arrange(.data$.original_order) |>
    dplyr::select(-".original_order")
}

assess_repeating <- function(x, max_repeats = 3) {
  stopifnot(is.numeric(max_repeats), max_repeats > 0, length(max_repeats) == 1)

  # replace NA's with the min - 1 (i.e. not repeating)
  default <- min(x, na.rm = TRUE) - 1
  is_missing <- is.na(x)
  x[is_missing] <- default

  # check for repetitions
  is_repeating <- !logical(length(x))
  for (lag_n in 1:max_repeats) {
    lagged <- dplyr::lag(x, n = lag_n, default = default)
    is_repeating <- is_repeating & x == lagged
  }

  # Mark the initial repeated values where repeats exists as repeating as well
  if (any(is_repeating)) {
    is_a_repeat <- 1:max_repeats |>
      lapply(\(n) which(is_repeating) - n) |>
      unlist() |>
      unique()
    is_a_repeat <- is_a_repeat[is_a_repeat > 0]
    is_repeating[is_a_repeat] <- TRUE
  }

  # mark originaly missing values as not repeating
  is_repeating[is_missing] <- FALSE
  return(is_repeating)
}

assess_spiking <- function(x, max_steps = list("1 hours" = Inf), time_step) {
  stopifnot(
    is.na(time_step) | !is.na(lubridate::as.period(time_step)),
    length(time_step) == 1
  )
  stopifnot(
    identical(class(max_steps), "list"),
    length(max_steps) > 0,
    all(lengths(max_steps) == 1),
    !anyNA(lubridate::as.period(names(max_steps)))
  )

  time_step <- lubridate::as.period(time_step)
  threshold_periods <- lapply(names(max_steps), lubridate::as.period)

  # ensure valid max_steps relative to time_step
  is_valid <- threshold_periods |>
    sapply(\(threshold_period) {
      ratio <- threshold_period / time_step
      as.integer(ratio) == ratio
    })
  stopifnot(
    "threshold periods (names of `max_steps`) must all be larger multiples of `time_step`" = all(
      is_valid
    )
  )

  # check if difference within each threshold period is greater than threshold
  is_spiking <- logical(length(x))
  for (i in seq_along(threshold_periods)) {
    threshold_period <- threshold_periods[[i]]
    lag_n <- threshold_period / time_step # number of steps to look back

    for (n in 1:lag_n) {
      is_spike_from_n <- (abs(x - dplyr::lag(x, n = n)) >= max_steps[[i]]) |>
        handyr::swap(NA, with = FALSE)
      is_spiking <- is_spiking | is_spike_from_n
    }
  }
  return(is_spiking)
}

assess_range <- function(x, range) {
  stopifnot(is.numeric(range), length(range) == 2)
  outside_range <- logical(length(x))
  is_outside <- x < min(range, na.rm = TRUE) | x > max(range, na.rm = TRUE)
  outside_range[is_outside] <- TRUE
  return(outside_range)
}

assess_missing <- function(x) {
  is.na(x)
}

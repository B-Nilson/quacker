assess_repeating <- function(x, max_repeats = 3) {
  stopifnot(is.numeric(max_repeats), max_repeats > 0, length(max_repeats) == 1)
  if (max_repeats >= length(x)) {
    return(logical(length(x)))
  }

  # replace NA's with the min - 1 (i.e. not repeating)
  default <- (handyr::min(x, na.rm = TRUE) - 1) |> 
    handyr::swap(NA, with = -1)
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
    !anyNA(lubridate::as.period(names(max_steps))),
    !anyNA(unlist(max_steps))
  )

  # Handle edge case where data cannot possibly be spiking
  smallest_threshold <- min(unlist(max_steps), na.rm = TRUE)
  largest_diff <- (handyr::max(x, na.rm = TRUE) - handyr::min(x, na.rm = TRUE)) |> 
    handyr::swap(NA, with = 0)
  if (largest_diff <= smallest_threshold) {
    return(logical(length(x)))
  }

  # parse time_step and threshold periods
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
  range <- sort(range)
  is_outside <- x < range[1] | x > range[2]
  is_outside[is.na(x)] <- FALSE
  return(is_outside)
}

assess_missing <- function(x) {
  is.na(x)
}

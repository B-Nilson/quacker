flag_bad_sensor <- function(
  hourly_values,
  allowed_range,
  max_steps,
  precision = 1,
  max_repeats = 3
) {
  stopifnot(max_repeats > 0)
  hourly_values <- hourly_values |> round(precision)
  # Following table 2 from: https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/met.1913

  ## Missingness
  is_missing <- is.na(hourly_values)

  ## Gross Error Limits
  is_out_of_range <- !(hourly_values |>
    dplyr::between(allowed_range[1], allowed_range[2])) |>
    handyr::swap(what = NA, with = FALSE)

  ## Temporal Consistency
  # https://www-sciencedirect-com.prxy.lib.unbc.ca/science/article/pii/S0022169411001594
  # Persistence: P(h) != P(h-1) != P(h-2) != P(h-3) ... (for 1:max_repeats)
  # i.e. flag if value repeats for  4+ hours (unless values near 0 are repeating and the 24hr abs max > 2)
  is_repeating <- hourly_values == dplyr::lag(hourly_values, default = -999)
  if (max_repeats > 1) {
    for (lag in 1:(max_repeats - 1)) {
      is_repeating <- is_repeating &
        hourly_values == dplyr::lag(hourly_values, n = lag, default = -999)
    }
  }
  low_threshold <- 1
  is_below_threshold <- abs(hourly_values) < low_threshold
  is_persistently_low <- is_below_threshold &
    lagged_max(abs(hourly_values), 1:23) < 2
  is_temporaly_inconsistent <- ((is_repeating & !is_below_threshold) |
    (is_repeating & is_persistently_low)) |>
    handyr::swap(what = NA, with = FALSE)

  # Step: P(h) - P(h-1) < 400, ...
  # i.e. flag if values jump up/down too quickly
  time_steps <- as.numeric(names(max_steps))
  is_jumping <- rep(FALSE, length(hourly_values))
  for (ts in time_steps) {
    is_jumping <- (is_jumping |
      abs(hourly_values - dplyr::lag(hourly_values, ts)) >=
        max_steps[[as.character(ts)]]) |>
      handyr::swap(what = NA, with = FALSE)
  }

  # Combine flags using binary
  is_missing +
    (2 * is_out_of_range) +
    (4 * is_temporaly_inconsistent) +
    (8 * is_jumping)
}

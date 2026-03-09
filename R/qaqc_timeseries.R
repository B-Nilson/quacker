# TODO: allow different ranges / repeats for different columns
# TODO: flag_bad_sensor tried to allow for persitently low repeating values, not sure if that's useful

#' Check for issues in timeseries data.
#'
#' @param ts_data A data frame with at least a date column and one or more value
#' columns.
#' @param date_col A tidyselect expression that resolves to a single column in `ts_data` with dates.
#'   Default is `NULL`, which will use the first date column from `ts_data`.
#' @param value_cols A tidyselect expression that resolves to one or more columns in `ts_data` with values to check.
#'   Default is `NULL`, which will use `dplyr::where(is.numeric)` to get all numeric columns from `ts_data`.
#' @param time_step A character string or a `Period` object, the time
#' step of the timeseries. If `NA` provided, the most common time step is determined using [handyr::get_step].
#' @param precision A numeric value, the precision to which `value_cols` are
#' rounded before assessments. Default is `Inf`, meaning no rounding is done.
#' @param rolling A character string or a `Period` object (such as `"3 hours"`), the time step of observations to average over for rolling average checks.
#' Default is `NA`, meaning no rolling average checking is done.
#' @param rolling_only A logical value, whether to only check rolling averages instead of the raw values. Default is `FALSE`.
#' @param allowed_range A numeric vector of length 2, the allowed range of
#' values. Default is `c(-Inf, Inf)`, meaning no range checking is done.
#' @param allowed_steps A named list, the allowed steps in the timeseries.
#' Each element of the list is a named numeric vector of length 1, the
#' name is the time step to look over and the value is the maximum allowed change within that time step.
#' Default is `list("1 hours" = Inf)`, meaning no step checking is done.
#' @param allowed_repeats A numeric value, the maximum allowed number of
#' consecutive repeated values. Default is `Inf`, meaning no repeat checking is done.
#'
#' @return A data frame with the same columns as `ts_data`, plus two columns
#' for each value column, the first containing a binary number combining the raised flags,
#' and the second containing a list of the individual raised flags.
#' @export
#' @importFrom rlang .data `:=`
qaqc_timeseries <- function(
  ts_data,
  date_col = NULL,
  value_cols = NULL,
  time_step = NA,
  precision = Inf,
  rolling = NA,
  rolling_only = FALSE,
  allowed_range = c(-Inf, Inf),
  allowed_steps = list("1 hours" = Inf),
  allowed_repeats = Inf
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
  stopifnot(is.logical(rolling_only), length(rolling_only) == 1)

  # try to guess date_col/value_cols if not provided, and handle tidyselect inputs
  date_col <- date_col |>
    guess_date_col(date_col = date_col) |>
    handle_tidyselect(data = ts_data, .expect_one = TRUE)
  value_cols <- value_cols |>
    guess_value_cols(data = ts_data) |>
    handle_tidyselect(data = ts_data, .expect_some = TRUE)

  # determine time_step from ts_data if not provided
  if (is.na(time_step)) {
    time_step <- handyr::get_step(ts_data[[date_col]])
  }

  # ensure valid rolling
  if (!is.na(rolling)) {
    rolling <- lubridate::as.period(rolling)
    stopifnot(
      "`rolling` must be interpretable as a `Period` object" = !is.na(rolling),
      "`rolling` must be a larger multiple of `time_step`" = (rolling /
        lubridate::as.period(time_step)) ==
        round(rolling / lubridate::as.period(time_step)),
      length(rolling) == 1
    )
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

  # fill time gaps, marking added rows
  ts_data <- ts_data |>
    dplyr::mutate(.original_order = dplyr::row_number()) |>
    gapfill_timeseries(date_col = date_col, time_step = time_step, sort = TRUE)

  # add rolling average columns if requested
  if (!is.na(rolling)) {
    rolling_width <- rolling / lubridate::as.period(time_step)
    rolling_name <- rolling |>
      as.character() |>
      gsub(pattern = " 0M| 0S", replacement = "")
    ts_data <- ts_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_cols),
          \(x) {
            x |>
              handyr::rolling(
                FUN = "mean",
                .width = rolling_width,
                .direction = "backward"
              ) |>
              round(digits = precision)
          },
          .names = "rolling_{.col}_" |> paste0(rolling_name)
        )
      )
    rolling_columns <- "rolling_" |>
      paste0(names(value_cols), "_", rolling_name) |>
      match(names(ts_data))
    names(rolling_columns) <- names(ts_data)[rolling_columns]

    if (rolling_only) {
      value_cols <- rolling_columns
    } else {
      value_cols[names(rolling_columns)] <- unname(rolling_columns)
    }
  }

  ts_data |>
    # round to precision and apply assessments to each value column
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(value_cols),
        \(x) if (is.integer(x)) x else round(x, precision)
      ),
      dplyr::across(
        dplyr::all_of(value_cols),
        assessments,
        .names = ".flag_{.col}_{.fn}"
      )
    ) |>
    # revert gap-filling and sorting
    dplyr::filter(!is.na(.data$.original_order)) |>
    dplyr::arrange(.data$.original_order) |>
    dplyr::select(-".original_order") |>
    # build combined binary flag column for each value
    # and move seperated flags to a list column
    combine_flags(
      value_cols = value_cols,
      flag_prefix = ".flag_",
      list_prefix = ".flags_"
    )
}

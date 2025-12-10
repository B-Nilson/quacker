combine_flags <- function(
  flagged_data,
  value_cols,
  flag_prefix = ".flag_",
  list_prefix = ".flags_",
  name_prefix = ".flag_name_"
) {
  stopifnot(is.data.frame(flagged_data))
  stopifnot(is.character(flag_prefix), length(flag_prefix) == 1)
  stopifnot(is.character(list_prefix), length(list_prefix) == 1)
  stopifnot(is.character(name_prefix), length(name_prefix) == 1)

  # handle tidyselect for value_cols
  value_cols <- value_cols |> tidyselect::eval_select(data = flagged_data)
  multiple_value_cols <- length(value_cols) > 0
  stopifnot(
    "`value_cols` must resolve to one or more columns in `flagged_data` using tidyselect (ex. c(value_a, value_b), c('value_a', 'value_b') or dplyr::starts_with('value_'))" = multiple_value_cols
  )

  # Ensure flag columns exist
  stopifnot(
    "Columns starting with `flag_prefix` + `value_cols` must exist in `flagged_data`" = any(startsWith(
      names(flagged_data),
      paste0(flag_prefix, names(value_cols))
    ))
  )

  # combine flags for each value column into a single integer binary value and a list column of individual flags
  for (value_col in names(value_cols)) {
    combined_col <- paste0(flag_prefix, value_col) # name for new combined column
    list_col <- paste0(list_prefix, value_col) # name for new list column
    name_col <- paste0(name_prefix, value_col) # name for new flag name column
    flags <- dplyr::starts_with(combined_col) |> # names of existing flag columns
      tidyselect::eval_select(data = flagged_data)
    flagged_data <- flagged_data |>
      # combine flags using binary (0/1 is first flag, 0/2 is second flag, 0/4 is third flag, etc.)
      # allows for a single integer number to be parsed back into the original flags
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
      # also move seperated flags to a list column to save on translating back if desired
      tidyr::nest(!!list_col := dplyr::any_of(names(flags))) |>
      # and combine flag names into a single name
      dplyr::mutate(
        !!name_col := .data[[list_col]] |>
          sapply(\(is_raised) {
            names(is_raised)[unlist(is_raised)] |>
              gsub(
                pattern = paste0(flag_prefix, value_col, "_is_"),
                replacement = ""
              ) |>
              gsub(pattern = "_", replacement = " ") |>
              paste(collapse = ", ") |>
              gsub(pattern = "repeating, repeating at range", replacement = "repeating at range") |>
              handyr::swap("", with = NA_character_)
          })
      )
  }
  return(flagged_data)
}

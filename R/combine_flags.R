combine_flags <- function(
  flagged_data,
  value_cols = NULL,
  flag_prefix = ".flag_",
  list_prefix = ".flags_",
  name_prefix = ".flag_name_"
) {
  stopifnot(is.data.frame(flagged_data))
  stopifnot(is.character(flag_prefix), length(flag_prefix) == 1)
  stopifnot(is.character(list_prefix), length(list_prefix) == 1)
  stopifnot(is.character(name_prefix), length(name_prefix) == 1)

  value_cols <- value_cols |>
    guess_value_cols(data = flagged_data) |>
    handle_tidyselect(data = flagged_data, .expect_some = TRUE)

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
    flags <- dplyr::starts_with(paste0(combined_col, "_is_")) |> # names of existing flag columns
      tidyselect::eval_select(data = flagged_data)
    flagged_data <- flagged_data |>
      # create combined binary flag column
      dplyr::mutate(
        !!combined_col := dplyr::across(dplyr::all_of(flags)) |>
          flags_to_binary_number()
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
              gsub(
                pattern = "repeating, repeating at range",
                replacement = "repeating at range"
              ) |>
              handyr::swap("", with = NA_character_)
          })
      )
  }
  return(flagged_data)
}

# Uses matrix multiplication to convert logical flags to a single integer (in binary)
# (0/1 is first flag, 0/2 is second flag, 0/4 is third flag, etc.)
flags_to_binary_number <- function(flags) {
  weights <- 2^(seq_len(ncol(flags)) - 1)
  as.integer(as.matrix(flags) %*% rev(weights))
}

flags_from_binary_number <- function(binary_numbers, flag_names) {
  seq_along(flag_names) |> 
    lapply(\(i) bitwAnd(binary_numbers, 2^(i - 1)) != 0) |> 
    stats::setNames(flag_names) |>
    dplyr::bind_cols()
}

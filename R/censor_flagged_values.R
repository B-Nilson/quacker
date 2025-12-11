censor_flagged_values <- function(
  qaqced_data,
  value_cols = NULL,
  censored_to = "{.col}_censored",
  flag_prefix = ".flag_"
) {
  value_cols <- value_cols |>
    guess_value_cols(data = qaqced_data) |>
    handle_tidyselect(data = qaqced_data, .expect_some = TRUE)

  qaqced_data |>
    dplyr::mutate(
      dplyr::across(
        value_cols,
        \(x) ifelse(get(paste0(flag_prefix, dplyr::cur_column())) == 0, x, NA),
        .names = censored_to
      )
    )
}

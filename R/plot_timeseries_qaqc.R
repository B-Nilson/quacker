#' @import patchwork
plot_timeseries_qaqc <- function(
  qaqced_ts_data,
  date_col = NULL,
  value_cols = NULL,
  time_step = NA,
  flag_prefix = ".flag_",
  log_scale_time_series = FALSE,
  log_scale_density = FALSE
) {
  date_col <- date_col |>
    guess_date_col(data = qaqced_ts_data) |>
    handle_tidyselect(data = qaqced_ts_data, .expect_one = TRUE)
  value_cols <- value_cols |>
    guess_value_cols(data = qaqced_ts_data) |>
    handle_tidyselect(data = qaqced_ts_data, .expect_some = TRUE)

  # Reformat for plotting
  plot_data <- qaqced_ts_data |>
    censor_flagged_values(value_cols = value_cols, flag_prefix = flag_prefix) |>
    tidyr::pivot_longer(
      cols = c(names(value_cols), paste0(names(value_cols), "_censored")),
      names_to = ".source",
      values_to = ".value"
    ) |>
    dplyr::mutate(
      is_censored = .source |>
        endsWith("_censored") |>
        ifelse("Censored", "Raw Data") |>
        factor(levels = c("Raw Data", "Censored")),
      .source = .source |> sub(pattern = "_censored", replacement = "")
    )

  # Make time series plot
  time_series <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = .value, color = .source)) +
    ggplot2::scale_x_datetime(expand = ggplot2::expansion(0)) +
    ggplot2::geom_line() +
    ggplot2::labs(colour = "Source")
  if (log_scale_time_series) {
    time_series <- time_series + ggplot2::scale_y_log10()
  }

  # Make density plot
  density <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = .value, color = .source)) +
    ggplot2::geom_density(show.legend = FALSE)
  if (log_scale_density) {
    density <- density + ggplot2::scale_x_log10()
  }

  # Combine plots
  (time_series | density) +
    patchwork::plot_layout(widths = c(3, 1), guides = "collect") &
    ggplot2::facet_wrap(is_censored ~ ., scales = "free", ncol = 1) &
    ggpubr::theme_pubr(border = TRUE) &
    ggplot2::theme(legend.position = "top")
}

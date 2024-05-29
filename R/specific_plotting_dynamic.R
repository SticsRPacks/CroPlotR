plot_dynamic_mixture <- function(df_data, sit) {
  ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$Plant)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$Plant),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ .data$variable)
}

plot_dynamic_mixture_overlap <- function(df_data, sit, overlap) {
  ggplot2::ggplot(
    df_data,
    ggplot2::aes(
      x = .data$Date,
      colour = paste(.data$Dominance, ": ", .data$Plant),
      shape = .data$Plant, linetype = .data$variable
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ .data$group_var, scales = "free") +
    ggplot2::labs(colour = "Plant", linetype = "Variable", shape = "Variable")
}

plot_dynamic_versions <- function(df_data, sit) {
  ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$version)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$version),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free")
}

plot_dynamic_overlap <- function(df_data, sit) {
  ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$variable)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$variable),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ .data$group_var, scales = "free") +
    ggplot2::labs(shape = "Variable", colour = "Variable")
}

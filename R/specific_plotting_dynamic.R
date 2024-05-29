plot_dynamic <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$variable)

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$Observed), na.rm = TRUE)

    if ("Obs_SD" %in% colnames(df_data)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$Observed - 2 * .data$Obs_SD,
            ymax = .data$Observed + 2 * .data$Obs_SD
          ),
          na.rm = TRUE
        )
    }
  }
  return(p)
}

plot_dynamic_mixture <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$Plant)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$variable)

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$Plant),
      na.rm = TRUE
    )
  }
  return(p)
}

plot_dynamic_mixture_overlap <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(
      x = .data$Date,
      colour = paste(.data$Dominance, ": ", .data$Plant),
      shape = .data$Plant, linetype = .data$variable
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$group_var, scales = "free") +
    ggplot2::labs(colour = "Plant", linetype = "Variable", shape = "Variable")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$Observed), na.rm = TRUE)
  }

  return(p)
}

plot_dynamic_versions <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$version)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$version),
      na.rm = TRUE
    )
  }
  return(p)
}

plot_dynamic_overlap <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$variable)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$group_var, scales = "free") +
    ggplot2::labs(shape = "Variable", colour = "Variable")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, shape = .data$variable),
      na.rm = TRUE
    )
  }
  return(p)
}

plot_dynamic_mixture_versions_overlap <- function(df_data, sit) {
  stop(
    "Too many cases to consider at a time: mixture + versions + overlap. ",
    "Please use only a maximum of two combinations of: ",
    "mixture, versions, overlap."
  )
}


plot_dynamic_versions_overlap <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(
      x = .data$Date, colour = .data$variable,
      linetype = .data$version
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$group_var, scales = "free") +
    ggplot2::labs(colour = "Variable")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed, colour = .data$variable),
      na.rm = TRUE
    )
  }
  return(p)
}

plot_dynamic_mixture_versions <- function(df_data, sit) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$Plant, linetype = .data$version)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed),
      na.rm = TRUE
    )
  }
  return(p)
}

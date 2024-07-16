#' Specific functions to generate dynamic plots
#'
#' @description Generate dynamic plots for the differents
#' cases handled in CroPlotR
#' (plant mixture, plot several simulation results on same graph, ...)
#' as specitifed by the different arguments.
#'
#' @inheritParams plot_situations
#'
#' @param df_data A named list of data frame including the data to plot (one df
#' per situation)
#' @param sit The name of the situation to plot
#' @param title The plot title (optional, NULL by default)
#'
#' @importFrom rlang .data
#' @return A ggplot object
#'
#' @details List of the different specific functions:
#' \itemize{
#'   \item `plot_dynamic_mixture`: Generate a dynamic plot for the case of
#' mixture of crops, single simulation version and each variable
#' in different plot
#'   \item `plot_dynamic_mixture_overlap`: Generate a dynamic plot for the
#' case of mixture of crops, single simulation version and several variables
#' in same plot (overlap)
#'   \item `plot_dynamic_versions`: Generate a dynamic plot for the
#' case of sole crops, all simulation versions in same plot and each
#' variable in a plot
#'   \item `plot_dynamic_overlap`: Generate a dynamic plot for the
#' case of sole crops, single simulation version and several variables
#' in same plot (overlap)
#'   \item `plot_dynamic_mixture_versions_overlap`: not implemented (seems not
#'   useful because too complex to analyze)
#'   \item `plot_dynamic_versions_overlap`: Generate a dynamic plot for the
#' case of sole crops, all simulation versions and several variables
#' in same plot (overlap)
#'   \item `plot_dynamic_mixture_versions`: Generate a dynamic plot for the
#' case of mixture of crops, all simulation versions in same plot and
#' each variable in a plot
#' }
#'
#' @return A list of ggplot objects
#'
#' @name specific_dynamic_plots
#'
NULL


#' @keywords internal
#' @rdname specific_dynamic_plots
plot_dynamic <- function(df_data, sit, title = NULL) {
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
  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_mixture <- function(df_data, sit, title = NULL) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, colour = .data$Plant)
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

  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_mixture_overlap <- function(df_data, sit, title = NULL) {
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

  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_versions <- function(df_data, sit, title = NULL) {
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
    if ("Obs_SD" %in% colnames(df_data)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$Observed - 2 * .data$Obs_SD,
            ymax = .data$Observed + 2 * .data$Obs_SD,
            shape = .data$version
          ),
          na.rm = TRUE
        )
    }
  }
  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_overlap <- function(df_data, sit, title = NULL) {
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
    if ("Obs_SD" %in% colnames(df_data)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$Observed - 2 * .data$Obs_SD,
            ymax = .data$Observed + 2 * .data$Obs_SD,
            shape = .data$version
          ),
          na.rm = TRUE
        )
    }
  }
  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_mixture_versions_overlap <- function(df_data, sit, title = NULL) {
  stop(
    "Too many cases to consider at a time: mixture + versions + overlap. ",
    "Please use only a maximum of two combinations of: ",
    "mixture, versions, overlap."
  )
}


plot_dynamic_versions_overlap <- function(df_data, sit, title = NULL) {
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
    if ("Obs_SD" %in% colnames(df_data)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$Observed - 2 * .data$Obs_SD,
            ymax = .data$Observed + 2 * .data$Obs_SD,
            colour = .data$variable
          ),
          na.rm = TRUE
        )
    }
  }
  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

plot_dynamic_mixture_versions <- function(df_data, sit, title = NULL) {
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(
      x = .data$Date,
      colour = .data$Plant,
      linetype = .data$version
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated)) +
    ggplot2::facet_wrap(~ .data$variable, scales = "free")

  if ("Observed" %in% colnames(df_data)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$Observed),
      na.rm = TRUE
    )
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
  p <- p +
    ggplot2::ggtitle(title)
  return(p)
}

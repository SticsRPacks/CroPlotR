#' Specific functions to generate dynamic plots
#'
#' @description Generate dynamic plots for the different
#' cases handled in CroPlotR
#' (plant mixture, plot several simulation results on same graph, ...)
#' as specified by the different arguments.
#'
#' @param df_data A named list of data frame including the data to plot (one df
#' per situation)
#' @param sit The name of the situation to plot
#' @param successive A list of lists containing the situations to be represented
#'  as a contiguous sequence
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

#' Builds a base dynamic plot showing simulated values as a line
#' and, when available, observed values as points with optional error bars.
#'
#' @param df_data Data frame containing at least a `Date` and a `Simulated`
#'   column. If an `Observed` column is present, observed values are overlaid as
#'   points. If an `Obs_SD` column is also present, error bars (±2 SD) are
#'   added.
#' @param title String. Title displayed at the top of the plot.
#' @param extra_aes Optional named list of additional aesthetics merged into
#'   the base plot mapping (e.g. colour, group). Defaults to NULL.
#' @param extra_obs_aes Optional named list of additional aesthetics merged into
#'   the observed points mapping. Defaults to NULL.
#'
#' @return A ggplot object representing the time series plot.
#'
#' @keywords internal
base_dyn_plot <- function(
  df_data, title, extra_aes = NULL, extra_obs_aes = NULL
) {
  # Build the base aesthetic mapping with Date on the x-axis
  final_aes <- ggplot2::aes(x = .data$Date)

  # Merge any provided extra aesthetics
  if (!is.null(extra_aes)) {
    final_aes <- modifyList(final_aes, extra_aes)
  }

  # Initialise the plot and draw simulated values as a line
  p <- ggplot2::ggplot(
    df_data,
    final_aes
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated))

  # Overlay observed values when the column is present in the data
  if ("Observed" %in% colnames(df_data)) {
    final_obs_aes <- ggplot2::aes(y = .data$Observed)

    # Merge any extra aesthetics for the observed points
    if (!is.null(extra_obs_aes)) {
      final_obs_aes <- modifyList(final_obs_aes, extra_obs_aes)
    }
    point_args <- list(
      mapping = final_obs_aes,
      na.rm = TRUE
    )

    if (is.null(final_obs_aes$colour)) {
      point_args$color <- "black"
    }

    p <- p + do.call(ggplot2::geom_point, point_args)

    # Add vertical error bars (±2 SD) when observation SD is available
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
  p + ggplot2::ggtitle(title)
}

#' @keywords internal
#' @rdname specific_dynamic_plots
plot_dynamic <- function(df_data, sit, successive, title = NULL) {
  p <- base_dyn_plot(df_data, title)
  p <- add_facet_wrap(p, var = "var", scales = "free_y")

  if (!is.null(successive)) {
    dates <- unique(df_data$succession_date)
    dates_vlines <- as.POSIXct(dates, tz = "UTC")

    p <- p + ggplot2::geom_vline(
      xintercept = dates_vlines[-length(dates_vlines)],
      linetype = "dashed",
      color = "black"
    )
  }
  return(p)
}

plot_dynamic_mixture <- function(df_data, sit, title = NULL) {
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(colour = paste(.data$Dominance, ":", .data$Plant)),
    extra_obs_aes = ggplot2::aes(
      colour = paste(.data$Dominance, ":", .data$Plant)
    )
  )
  p <- add_facet_wrap(p, var = "var", scales = "free_y")

  p <- p +
    ggplot2::labs(colour = "Plant")
  return(p)
}

plot_dynamic_mixture_overlap <- function(df_data, sit, title = NULL) {
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(
      colour = .data$var,
      linetype = paste(.data$Dominance, ": ", .data$Plant),
      shape = paste(.data$Dominance, ": ", .data$Plant)
    ),
    extra_obs_aes = ggplot2::aes(
      shape = paste(.data$Dominance, ": ", .data$Plant),
      color = .data$var
    )
  )
  p <- add_facet_wrap(p, var = "group_var", scales = "free")

  p <- p +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = "Variable"),
      # add override.aes = list(shape = NA) in prev guide_legend?
      linetype = ggplot2::guide_legend(title = "Plant", order = 1),
      shape = ggplot2::guide_legend(title = "Plant", order = 1)
    )
  return(p)
}

plot_dynamic_versions <- function(df_data, sit, title = NULL) {
  df_data$Observed_Legend <- "Observed Value"
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(colour = .data$version),
    extra_obs_aes = ggplot2::aes(
      shape = .data$Observed_Legend
    )
  )
  p <- add_facet_wrap(p, var = "var", scales = "free")

  p <- p +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        title = "Version",
        override.aes = list(shape = NA)
      ),
      shape = ggplot2::guide_legend(title = "Observations")
    )
  return(p)
}

plot_dynamic_overlap <- function(df_data, sit, title = NULL) {
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(colour = .data$var),
    extra_obs_aes = ggplot2::aes(
      shape = .data$var,
      colour = .data$var
    )
  )
  p <- add_facet_wrap(p, var = "group_var", scales = "free")

  if ("Observed" %in% colnames(df_data)) {
    p <- p +
      ggplot2::labs(shape = "Variable")
  }
  p <- p + ggplot2::labs(colour = "Variable")

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
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(
      colour = .data$var,
      linetype = .data$version
    ),
    extra_obs_aes = ggplot2::aes(
      colour = .data$var
    )
  )
  p <- add_facet_wrap(p, var = "group_var", scales = "free")

  p <- p +
    ggplot2::labs(colour = "Variable", linetype = "Version")

  return(p)
}

plot_dynamic_mixture_versions <- function(df_data, sit, title = NULL) {
  p <- base_dyn_plot(
    df_data,
    title,
    extra_aes = ggplot2::aes(
      colour = paste(.data$Dominance, ":", .data$Plant),
      linetype = .data$version
    ),
    extra_obs_aes = ggplot2::aes(
      colour = paste(.data$Dominance, ":", .data$Plant)
    )
  )
  p <- add_facet_wrap(p, var = "var", scales = "free")

  p <- p +
    ggplot2::labs(
      colour = "Plant",
      linetype = "Version"
    )
  return(p)
}

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

#' @keywords internal
#' @description Add vertical lines between situations in case of successive situations.
#' @rdname specific_dynamic_plots
#' @param p A ggplot to modify`
#' @return A ggplot object with vertical lines added if successive situations are provided.
add_vertical_lines <- function(df_data, successive, p) {
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

#' @keywords internal
#' @description Make a multiline title for a ggplot object, splitting the title into multiple lines (for successive situations only) if it exceeds a certain character limit.
#' @rdname specific_dynamic_plots
#' @param title A character string representing the title to be split into multiple lines.
#' @param max_char An integer specifying the maximum number of characters per line. Default is 80
#' @return A character string with newline characters inserted to create a multiline title.
make_multiline_title <- function(title, max_char = 120) {
  labels <- strsplit(title, " \\| ")[[1]]

  lines <- character()
  current <- labels[1]

  if (length(labels) > 1) {
    for (lab in labels[-1]) {
      candidate <- paste(current, lab, sep = " | ")

      if (nchar(candidate) <= max_char) {
        current <- candidate
      } else {
        lines <- c(lines, paste0(current, " ..."))
        current <- lab
      }
    }
  }

  lines <- c(lines, current)

  paste(lines, collapse = "\n")
}

dynamic_plot_spec <- function(df_data, mixture, one_version, overlap) {
  spec_name <- paste(
    if (mixture) "mixture" else "sole",
    if (one_version) "one_version" else "versions",
    if (is.null(overlap)) "no_overlap" else "overlap",
    sep = "_"
  )

  switch(
    spec_name,
    mixture_one_version_no_overlap = list(
      mapping = ggplot2::aes(colour = paste(.data$Dominance, ":", .data$Plant)),
      scales = "free_y", facets = "var",
      extra = list(ggplot2::labs(colour = "Plant")),
      legend_labels = unique(paste(df_data$Dominance, ":", df_data$Plant))
    ),
    mixture_one_version_overlap = list(
      mapping = ggplot2::aes(
        colour = .data$var,
        linetype = paste(.data$Dominance, ": ", .data$Plant),
        shape = paste(.data$Dominance, ": ", .data$Plant)
      ),
      facets = "group_var",
      obs_mapping = ggplot2::aes(
        shape = paste(.data$Dominance, ": ", .data$Plant),
        color = .data$var
      ),
      extra = list(
        ggplot2::guides(
          colour = ggplot2::guide_legend(title = "Variable"),
          linetype = ggplot2::guide_legend(title = "Plant", order = 1),
          shape = ggplot2::guide_legend(title = "Plant", order = 1)
        )
      ),
      legend_labels = c(
        unique(as.character(df_data$var)),
        unique(paste(df_data$Dominance, ": ", df_data$Plant))
      )
    ),
    sole_one_version_no_overlap = list(scales = "free_y", facets = "var"),
    sole_one_version_overlap = list(
      mapping = ggplot2::aes(colour = .data$var),
      facets = "group_var",
      obs_mapping = ggplot2::aes(shape = .data$var),
      extra = list(
        ggplot2::labs(colour = "Variable", shape = "Variable")
      ),
      legend_labels = unique(as.character(df_data$var))
    ),
    sole_versions_no_overlap = list(
      mapping = ggplot2::aes(colour = .data$version),
      facets = "var",
      obs_mapping = ggplot2::aes(shape = "Observed Value"),
      obs_params = list(color = "black"),
      extra = list(
        ggplot2::guides(
          colour = ggplot2::guide_legend(
            title = "Version",
            override.aes = list(shape = NA)
          ),
          shape = ggplot2::guide_legend(title = "Observations")
        )
      ),
      legend_labels = c(unique(df_data$version), "Observed Value")
    ),
    sole_versions_overlap = list(
      mapping = ggplot2::aes(
        colour = .data$var,
        linetype = .data$version
      ),
      facets = "group_var",
      obs_mapping = ggplot2::aes(colour = .data$var),
      extra = list(
        ggplot2::labs(colour = "Variable", linetype = "Version")
      ),
      legend_labels = c(
        unique(as.character(df_data$var)),
        unique(df_data$version)
      )
    ),
    mixture_versions_no_overlap = list(
      mapping = ggplot2::aes(
        colour = paste(.data$Dominance, ":", .data$Plant),
        linetype = .data$version
      ),
      facets = "var",
      extra = list(
        ggplot2::labs(
          colour = "Plant",
          linetype = "Version"
        )
      ),
      legend_labels = c(
        unique(paste(df_data$Dominance, ":", df_data$Plant)),
        unique(df_data$version)
      )
    ),
    mixture_versions_overlap = stop(
      "Too many cases to consider at a time: mixture + versions + overlap. ",
      "Please use only a maximum of two combinations of: ",
      "mixture, versions, overlap."
    )
  )
}

build_dynamic_plot <- function(df_data, sit, successive, title = NULL,
                               mixture, one_version, overlap) {
  spec <- dynamic_plot_spec(df_data,mixture, one_version, overlap)
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(x = .data$Date, !!!spec$mapping)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Simulated))

  p <- add_vertical_lines(df_data, successive, p)

  if ("Observed" %in% colnames(df_data)) {
    p <- p + do.call(
      ggplot2::geom_point,
      c(
        list(
          mapping = ggplot2::aes(y = .data$Observed, !!!spec$obs_mapping),
          na.rm = TRUE
        ),
        spec$obs_params
      )
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
    ggplot2::ggtitle(make_multiline_title(title)) +
    ggplot2::theme_get() +
    spec$extra

  p <- add_facet_wrap(p, spec$facets, spec$scales, legend_labels = spec$legend_labels)

  p
}

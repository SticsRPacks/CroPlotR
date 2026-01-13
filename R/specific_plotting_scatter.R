#' Specific functions to generate scatter plots
#'
#' @description Generate scatter plots for the different cases handled in
#' CroPlotR (plant mixture, plot of residuals, plot several simulation results
#' on same graph, ...) as specified by the different arguments.
#'
#' @param df_data A named list of data frame including the data to plot (one df
#' per situation, or only one df if sit==all_situations)
#' @param sit The name of the situation to plot (or all_situations)
#' @param is_obs_sd TRUE if error standard deviation of observations is provided
#' @param mixture TRUE if the plot is for a mixture of crops
#' @param one_version TRUE if the plot is for one version
#' @param has_distinct_situations TRUE if the plot is for several situations
#'
#' @importFrom rlang .data
#' @return A ggplot object
#'
#' @details List of the different specific functions:
#' \itemize{
#'   \item `plot_scat_mixture_allsit`: Generate a scatter plot for the case of
#' mixture of crops, single simulation version and all_situations in same plot
#'   \item `plot_scat_allsit`: Generate a scatter plot for the case of
#' sole crops, single simulation version and all_situations in same plot
#' }
#'
#' @return A list of ggplot objects
#'
#' @name specific_scatter_plots
#'
NULL


#' @keywords internal
#' @description Compute axis bounds (+/-0.05 added to the min/max of the data).
#' @rdname specific_scatter_plots
#' @param y_var_type type of variable to plot ("Simulated" or "Residuals")
#' @return List of x and y axis bounds (xaxis_min, xaxis_max, yaxis_min,
#' yaxis_max)
compute_axis_bounds <- function(df_data, reference_var, y_var_type, is_obs_sd) {
  # Compute x and y axis min and max to set axis limits
  df_min <- df_data %>%
    group_by(.data$var) %>%
    summarise(across(where(is.numeric), min))
  df_max <- df_data %>%
    group_by(.data$var) %>%
    summarise(across(where(is.numeric), max))
  xaxis_min <- df_min[[reference_var]] - 0.05 * df_min[[reference_var]]
  xaxis_max <- df_max[[reference_var]] + 0.05 * df_max[[reference_var]]
  yaxis_min <- df_min[[y_var_type]] - 0.05 * df_min[[y_var_type]]
  yaxis_max <- df_max[[y_var_type]] + 0.05 * df_max[[y_var_type]]

  if (is_obs_sd && reference_var == "Observed") {
    # Update xaxis min and max in case of addition of error bars
    df_min <- df_data %>%
      mutate(barmin = .data$Observed - 2 * .data$Obs_SD) %>%
      group_by(.data$var) %>%
      summarise(across(where(is.numeric), min))
    df_max <- df_data %>%
      mutate(barmax = .data$Observed + 2 * .data$Obs_SD) %>%
      group_by(.data$var) %>%
      summarise(across(where(is.numeric), max))
    xaxis_min <- df_min[["barmin"]] - 0.05 * df_min[["barmin"]]
    xaxis_max <- df_max[["barmax"]] + 0.05 * df_max[["barmax"]]
  }

  return(list(
    xaxis_min = xaxis_min, xaxis_max = xaxis_max,
    yaxis_min = yaxis_min, yaxis_max = yaxis_max
  ))
}


#' @keywords internal
#' @description Make axis square
#' @rdname specific_scatter_plots
#' @param p A ggplot to modify`
#' @param y_var_type type of variable to plot ("Simulated" or "Residuals")
#' @return The modified ggplot
make_axis_square <- function(df_data, reference_var, y_var_type, is_obs_sd, p) {
  axis_bounds <- compute_axis_bounds(df_data, reference_var, y_var_type, is_obs_sd)
  axis_min <- pmin(axis_bounds$xaxis_min, axis_bounds$yaxis_min)
  axis_max <- pmax(axis_bounds$xaxis_max, axis_bounds$yaxis_max)
  p <- p +
    ggh4x::facetted_pos_scales(
      x = lapply(1:length(axis_min), function(i) {
        ggplot2::scale_x_continuous(limits = c(axis_min[i], axis_max[i]))
      }),
      y = lapply(1:length(axis_min), function(i) {
        ggplot2::scale_y_continuous(limits = c(axis_min[i], axis_max[i]))
      })
    )
  return(p)
}

#' @keywords internal
#' @description Ensure that the Y axis includes zero when all values in a facet
#' are strictly positive or strictly negative.
#' @rdname specific_scatter_plots
#' @param p A ggplot to modify`
#' @param y_var_type type of variable to plot ("Simulated" or "Residuals")
#' @return The modified ggplot
force_y_axis <- function(df_data, reference_var, y_var_type, is_obs_sd, p) {
  axis_bounds <- compute_axis_bounds(
    df_data,
    reference_var,
    y_var_type,
    is_obs_sd
  )
  y_min <- axis_bounds$yaxis_min
  y_max <- axis_bounds$yaxis_max

  # New y limits per facet
  y_min_new <- ifelse(y_min > 0, 0, y_min)
  y_max_new <- ifelse(y_max < 0, 0, y_max)

  p +
    ggh4x::facetted_pos_scales(
      y = lapply(seq_along(y_min_new), function(i) {
        ggplot2::scale_y_continuous(limits = c(y_min_new[i], y_max_new[i]))
      })
    )
}

#' Get reference variable for plotting
#'
#' @description Return the reference variable and its display name for scatter
#' plots
#'
#' @param reference_var The reference variable name, if NULL "Observed" is used
#'
#' @keywords internal
#'
#' @return A list with two elements:
#' \itemize{
#'   \item reference_var: The reference variable name: "Observed" or "Reference"
#'   \item reference_var_name: The display name for the reference variable
#' }
give_reference_var <- function(reference_var) {
  if (is.null(reference_var)) {
    reference_var <- "Observed"
    reference_var_name <- "Observed"
  } else {
    reference_var_name <- reference_var
    reference_var <- "Reference"
  }
  return(
    list(
      reference_var = reference_var, reference_var_name = reference_var_name
    )
  )
}

#' Get y variable type for plotting
#'
#' @description Return the type of y variable for scatter plots based on
#' selection
#'
#' @param select_scat Selection type, either "sim" for Simulated or any other
#' value for Residuals
#'
#' @keywords internal
#'
#' @return A character string, either "Simulated" or "Residuals"
give_y_var_type <- function(select_scat) {
  if (select_scat == "sim") {
    y_var_type <- "Simulated"
  } else {
    y_var_type <- "Residuals"
  }
  return(y_var_type)
}


#' @keywords internal
#' @description Add error bars on observed values in given scatterplot
#' @rdname specific_scatter_plots
#' @param p A ggplot to modify`
#' @param colour_factor The factor to use for colouring the error bars
#' @return The modified ggplot
add_obs_error_bars <- function(p, colour_factor = NULL) {
  p <- p +
    ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = .data$Observed - 2 * .data$Obs_SD,
        xmax = .data$Observed + 2 * .data$Obs_SD,
        colour = .data[[colour_factor]],
      ),
      na.rm = TRUE
    )
  return(p)
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_allsit <- function(df_data, sit, select_scat, shape_sit,
                                     reference_var, is_obs_sd, title = NULL) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name
      )
    )

  if (shape_sit == "none" || shape_sit == "txt") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
      ),
      na.rm = TRUE
    )
  } else if (shape_sit == "symbol" || shape_sit == "group") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(paste(.data$Dominance, ":", .data$Plant)),
        shape = as.factor(paste(.data$sit_name))
      ),
      na.rm = TRUE
    ) +
      ggplot2::scale_shape_discrete(name = "Situation")
  }

  p <- p +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = .data[[y_var_type]], x = .data[[reference_var]]),
      inherit.aes = FALSE,
      method = lm, color = "blue",
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::facet_wrap(~ .data$var, scales = "free")

  p <- p +
    ggplot2::ggtitle(title)

  if (is_obs_sd && reference_var == "Observed") {
    p$data$colour_factor <- as.factor(paste(p$data$Dominance, ":", p$data$Plant))
    p <- add_obs_error_bars(p, colour_factor = "colour_factor")
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(
          colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
        ),
        show.legend = FALSE,
        max.overlaps = 100
      )
  }

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }

  p <- p + ggplot2::scale_color_discrete(name = "Plant")

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_versions <- function(df_data, sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name
      )
    )

  if (shape_sit == "none" || shape_sit == "txt") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        shape = as.factor(paste(.data$Dominance, ":", .data$Plant)),
        colour = as.factor(.data$version)
      ),
      na.rm = TRUE
    ) +
      ggplot2::labs(color = "Version", shape = "Plant")
  } else if (shape_sit == "symbol" || shape_sit == "group") {
    # ! In this case we loose the colour by species for mixtures, because
    # there would be three aesthetics to handle (situation, version and
    # species). We made this decision because the user explicitly asks
    # for shape to be the situation name. If they want to color by species,
    # they can put shape_sit = "none" or shape_sit = "txt" to have it all.
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(.data$version),
        shape = as.factor(.data$sit_name)
      ),
      na.rm = TRUE
    ) +
      ggplot2::labs(color = "Version", shape = "Situation")
  }

  p <- p +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = .data[[y_var_type]], x = .data[[reference_var]]),
      inherit.aes = FALSE,
      method = lm, color = "blue",
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::facet_wrap(~ .data$var, scales = "free")

  p <- p + ggplot2::ggtitle(title)

  if (is_obs_sd && reference_var == "Observed") {
    p <- p +
      ggplot2::geom_linerange(
        ggplot2::aes(
          xmin = .data$Observed - 2 * .data$Obs_SD,
          xmax = .data$Observed + 2 * .data$Obs_SD,
          colour = as.factor(.data$version),
        ),
        na.rm = TRUE
      )
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(
          colour = as.factor(.data$version)
        ),
        show.legend = FALSE,
        max.overlaps = 100
      )
  }

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_allsit <- function(df_data, sit, select_scat, shape_sit,
                             reference_var, is_obs_sd, title = NULL,
                             has_distinct_situations = FALSE,
                             one_version = FALSE, mixture = FALSE) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)
  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))
  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name
      )
    )

  if (shape_sit == "none" || shape_sit == "txt") {
    p <- p + ggplot2::geom_point(na.rm = TRUE)
  } else if (shape_sit == "symbol" || shape_sit == "group") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(paste(.data$sit_name))
      ),
      na.rm = TRUE
    ) +
      ggplot2::scale_color_discrete(name = "Situation")
  }

  p <- p +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(y = .data[[y_var_type]], x = .data[[reference_var]]),
      inherit.aes = FALSE,
      method = lm, color = "blue",
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::facet_wrap(~ .data$var, scales = "free")

  p <- p +
    ggplot2::ggtitle(title)

  if (is_obs_sd && reference_var == "Observed") {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = .data$Observed - 2 * .data$Obs_SD,
        xmax = .data$Observed + 2 * .data$Obs_SD,
      ),
      na.rm = TRUE
    )
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p + ggrepel::geom_text_repel(max.overlaps = 100)
  }

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (
    has_distinct_situations == FALSE &&
      one_version == TRUE &&
      mixture == FALSE
  ) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_per_sit <- function(df_data,
                                       sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name,
      )
    )

  p <- p + ggplot2::geom_point(
    ggplot2::aes(colour = as.factor(.data$version)),
    na.rm = TRUE
  ) +
    ggplot2::labs(color = "Version")
  p <- p +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        colour = as.factor(.data$version)
      ),
      method = lm,
      inherit.aes = FALSE,
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::facet_wrap(~ .data$var, scales = "free")

  p <- p + ggplot2::ggtitle(title) +
    if (shape_sit == "txt") {
      ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$sit_name),
        na.rm = TRUE,
        show.legend = FALSE,
        max.overlaps = Inf
      )
    }

  if (is_obs_sd && reference_var == "Observed") {
    p <- p +
      ggplot2::geom_linerange(
        ggplot2::aes(
          xmin = .data$Observed - 2 * .data$Obs_SD,
          xmax = .data$Observed + 2 * .data$Obs_SD,
          colour = as.factor(.data$version),
        ),
        na.rm = TRUE
      )
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_allsit <- function(df_data,
                                      sit, select_scat, shape_sit,
                                      reference_var, is_obs_sd, title = NULL) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name
      )
    )

  if (shape_sit == "none" || shape_sit == "txt") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(.data$version)
      ),
      na.rm = TRUE
    ) +
      ggplot2::labs(color = "Version")
  } else if (shape_sit == "symbol" || shape_sit == "group") {
    # ! In this case we loose the colour by species for mixtures, because
    # there would be three aesthetics to handle (situation, version and
    # species). We made this decision because the user explicitly asks
    # for shape to be the situation name. If they want to color by species,
    # they can put shape_sit = "none" or shape_sit = "txt" to have it all.
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(.data$version),
        shape = as.factor(.data$sit_name)
      ),
      na.rm = TRUE
    ) +
      ggplot2::labs(color = "Version", shape = "Situation")
  }

  p <- p +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        colour = as.factor(.data$version)
      ),
      inherit.aes = FALSE,
      method = lm,
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::facet_wrap(~ .data$var, scales = "free")

  p <- p + ggplot2::ggtitle(title)

  if (is_obs_sd && reference_var == "Observed") {
    p <- p +
      ggplot2::geom_linerange(
        ggplot2::aes(
          xmin = .data$Observed - 2 * .data$Obs_SD,
          xmax = .data$Observed + 2 * .data$Obs_SD,
          colour = as.factor(.data$version),
        ),
        na.rm = TRUE
      )
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(colour = as.factor(.data$version)),
        show.legend = FALSE,
        max.overlaps = 100
      )
  }

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }

  return(p)
}

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
#' @rdname specific_scatter_plots
#' @description
#' Retrieve the actual display order of facet panels from a ggplot object.
#'
#' This function builds the ggplot object and extracts the layout table
#' to determine the order in which facet panels are rendered. The returned
#' order corresponds exactly to the visual arrangement of panels in the plot,
#' which may differ from the order of factor levels in the original data.
#'
#' @param p A ggplot object containing faceting.
#' @param facet_var Character string giving the name of the faceting variable
#'   (default is `"var"`).
#'
#' @return A character vector giving the facet levels in their display order.
#'
#' @details
#' The function relies on \code{ggplot2::ggplot_build()} to compute the plot
#' layout and extracts the panel structure from the resulting object.
#'
get_facet_order <- function(p, facet_var = "var") {
  gb <- ggplot2::ggplot_build(p)
  layout_df <- gb$layout$layout

  layout_df <- layout_df[order(layout_df$PANEL), ]

  as.character(layout_df[[facet_var]])
}


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

  vars <- df_min$var
  names(xaxis_min) <- vars
  names(xaxis_max) <- vars
  names(yaxis_min) <- vars
  names(yaxis_max) <- vars

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
  facet_order <- get_facet_order(p)

  axis_min <- pmin(
    axis_bounds$xaxis_min[facet_order],
    axis_bounds$yaxis_min[facet_order]
  )

  axis_max <- pmax(
    axis_bounds$xaxis_max[facet_order],
    axis_bounds$yaxis_max[facet_order]
  )
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
  # Recover actual facet display order
  facet_order <- get_facet_order(p)

  # Reorder y limits according to plot layout
  y_min <- axis_bounds$yaxis_min[facet_order]
  y_max <- axis_bounds$yaxis_max[facet_order]


  expand_range <- function(min, max, mult = 0.05) {
    delta <- max - min
    if (delta == 0) delta <- abs(min) + 1e-9
    c(min - delta * mult, max + delta * mult)
  }

  lims <- Map(function(lo, hi) {
    base_min <- min(lo, 0)
    base_max <- max(hi, 0)

    expand_range(base_min, base_max, 0.05)
  }, y_min, y_max)

  p +
    ggh4x::facetted_pos_scales(
      y = lapply(lims, function(l) {
        ggplot2::scale_y_continuous(limits = l)
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
#' @description Build a scatter plot shared by all the specific scatter plot
#' functions.
#' @rdname specific_scatter_plots
#' @param mapping Aesthetic mapping of the points (colour and/or shape). When
#' `shape_sit` is "symbol" or "group", the situation is added on the shape, or
#' on the colour if `mapping` has no colour (it replaces any existing shape).
#' Error bars and text labels use the same colour as the points.
#' @param smooth_by_colour If TRUE, one regression line per point colour,
#' otherwise a single blue regression line.
#' @param extra List of additional ggplot components (labs, scales...).
#' @param legend_labels Named list (by aesthetic) of the labels displayed in the
#' legend, passed to `add_facet_wrap`.
#' @return A ggplot object
build_scatter_plot <- function(
  df_data, select_scat, shape_sit, reference_var, is_obs_sd, title = NULL,
  mapping = ggplot2::aes(), smooth_by_colour = FALSE, extra = NULL,
  legend_labels = list()
) {
  tmp <- give_reference_var(reference_var)
  reference_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name
  y_var_type <- give_y_var_type(select_scat)

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y_var_type]]))

  if (shape_sit %in% c("symbol", "group")) {
    sit_aes <- if (is.null(mapping$colour)) "colour" else "shape"
    mapping[[sit_aes]] <- rlang::quo(as.factor(.data$sit_name))
    extra <- c(extra, list(ggplot2::labs(!!sit_aes := "Situation")))
    legend_labels[[sit_aes]] <- unique(df_data$sit_name)
  }

  smooth_aes <- ggplot2::aes(y = .data[[y_var_type]], x = .data[[reference_var]])
  smooth_params <- list(colour = "blue")
  if (smooth_by_colour) {
    smooth_aes$colour <- mapping$colour
    smooth_params <- list()
  }

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(
        y = .data[[y_var_type]], x = .data[[reference_var]],
        label = .data$sit_name
      )
    ) +
    ggplot2::geom_point(mapping, na.rm = TRUE) +
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    do.call(
      ggplot2::geom_smooth,
      c(
        list(
          mapping = smooth_aes,
          inherit.aes = FALSE,
          method = lm,
          se = FALSE, linewidth = 0.6, formula = y ~ x,
          fullrange = TRUE, na.rm = TRUE
        ),
        smooth_params
      )
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::ggtitle(title)

  if (is_obs_sd && reference_var == "Observed") {
    error_aes <- ggplot2::aes(
      xmin = .data$Observed - 2 * .data$Obs_SD,
      xmax = .data$Observed + 2 * .data$Obs_SD
    )
    error_aes$colour <- mapping$colour
    p <- p + ggplot2::geom_linerange(error_aes, na.rm = TRUE)
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    text_aes <- ggplot2::aes()
    text_aes$colour <- mapping$colour
    p <- p +
      ggrepel::geom_text_repel(
        text_aes,
        show.legend = FALSE,
        max.overlaps = 100
      )
  }

  p <- p + extra

  p <- add_facet_wrap(
    p,
    var = "var", scales = "free",
    legend_labels = unlist(legend_labels, use.names = FALSE)
  )

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" && reference_var == "Observed") {
    p <- make_axis_square(df_data, reference_var, y_var_type, is_obs_sd, p)
  }
  if (select_scat == "res") {
    p <- force_y_axis(df_data, reference_var, y_var_type, is_obs_sd, p)
  }

  p
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_allsit <- function(df_data, sit, select_scat, shape_sit,
                                     reference_var, is_obs_sd, title = NULL) {
  build_scatter_plot(
    df_data, select_scat, shape_sit, reference_var, is_obs_sd, title,
    mapping = ggplot2::aes(
      colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
    ),
    extra = list(ggplot2::labs(colour = "Plant")),
    legend_labels = list(
      colour = unique(paste(df_data$Dominance, ":", df_data$Plant))
    )
  )
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_versions <- function(df_data, sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  build_scatter_plot(
    df_data, select_scat, shape_sit, reference_var, is_obs_sd, title,
    # ! With shape_sit = "symbol" or "group", we loose the shape by species for
    # mixtures, because there would be three aesthetics to handle (situation,
    # version and species). We made this decision because the user explicitly
    # asks for shape to be the situation name. If they want the species,
    # they can put shape_sit = "none" or shape_sit = "txt" to have it all.
    mapping = ggplot2::aes(
      colour = as.factor(.data$version),
      shape = as.factor(paste(.data$Dominance, ":", .data$Plant))
    ),
    smooth_by_colour = TRUE,
    extra = list(ggplot2::labs(colour = "Version", shape = "Plant")),
    legend_labels = list(
      colour = unique(df_data$version),
      shape = unique(paste(df_data$Dominance, ":", df_data$Plant))
    )
  )
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_allsit <- function(df_data, sit, select_scat, shape_sit,
                             reference_var, is_obs_sd, title = NULL,
                             has_distinct_situations = FALSE,
                             one_version = FALSE, mixture = FALSE) {
  p <- build_scatter_plot(
    df_data, select_scat, shape_sit, reference_var, is_obs_sd, title
  )

  if (
    has_distinct_situations == FALSE &&
      one_version == TRUE &&
      mixture == FALSE
  ) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_per_sit <- function(df_data,
                                       sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  build_scatter_plot(
    df_data, select_scat,
    # Only one situation per plot: no need for the situation in the legend
    shape_sit = if (shape_sit == "txt") "txt" else "none",
    reference_var, is_obs_sd, title,
    mapping = ggplot2::aes(colour = as.factor(.data$version)),
    smooth_by_colour = TRUE,
    extra = list(ggplot2::labs(colour = "Version")),
    legend_labels = list(colour = unique(df_data$version))
  )
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_allsit <- function(df_data,
                                      sit, select_scat, shape_sit,
                                      reference_var, is_obs_sd, title = NULL) {
  build_scatter_plot(
    df_data, select_scat, shape_sit, reference_var, is_obs_sd, title,
    mapping = ggplot2::aes(colour = as.factor(.data$version)),
    smooth_by_colour = TRUE,
    extra = list(ggplot2::labs(colour = "Version")),
    legend_labels = list(colour = unique(df_data$version))
  )
}

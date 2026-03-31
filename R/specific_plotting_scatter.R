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

  list(
    xaxis_min = xaxis_min, xaxis_max = xaxis_max,
    yaxis_min = yaxis_min, yaxis_max = yaxis_max
  )
}


#' @keywords internal
#' @description Make axis square
#' @rdname specific_scatter_plots
#' @param p A ggplot to modify`
#' @param y_var_type type of variable to plot ("Simulated" or "Residuals")
#' @return The modified ggplot
make_axis_square <- function(df_data, reference_var, y_var_type, is_obs_sd, p) {
  axis_bounds <- compute_axis_bounds(
    df_data, reference_var, y_var_type, is_obs_sd
  )
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
      x = lapply(seq_along(axis_min), function(i) {
        ggplot2::scale_x_continuous(limits = c(axis_min[i], axis_max[i]))
      }),
      y = lapply(seq_along(axis_min), function(i) {
        ggplot2::scale_y_continuous(limits = c(axis_min[i], axis_max[i]))
      })
    )
  p
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
  list(
    reference_var = reference_var, reference_var_name = reference_var_name
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
    return("Simulated")
  }
  "Residuals"
}

#' Adds a regression line to an existing ggplot object.
#'
#' @param p A ggplot object to add the regression line to.
#' @param x_var String. Name of the column to use as the x-axis variable.
#' @param y_var String. Name of the column to use as the y-axis variable.
#' @param extra_aesthetics Optional named list of additional aesthetics to merge
#'   into the line mapping (e.g. colour, group). Defaults to NULL.
#' @param line_color String. Colour of the regression line when no colour
#'   aesthetic is set via `extra_aesthetics`. Defaults to `"blue"`.
#'
#' @return The input ggplot object with a linear regression line appended.
#'
#' @keywords internal
add_regression_line <- function(
  p, x_var, y_var, extra_aesthetics = NULL, line_color = "blue"
) {
  # Build the base aesthetic mapping for the regression line
  line_aes <- ggplot2::aes(
    y = .data[[y_var]], x = .data[[x_var]]
  )

  # Merge any provided extra aesthetics
  if (!is.null(extra_aesthetics)) {
    line_aes <- modifyList(line_aes, extra_aesthetics)
  }

  # Assemble arguments for geom_smooth (OLS, no confidence ribbon)
  smooth_args <- list(
    mapping = line_aes,
    inherit.aes = FALSE,
    method = lm,
    se = FALSE,
    linewidth = 0.6,
    formula = y ~ x,
    fullrange = TRUE,
    na.rm = TRUE
  )

  # Fall back to blue when no colour aesthetic has been set
  if (is.null(line_aes$colour)) {
    smooth_args$color <- line_color
  }

  # Append the regression line layer and return the updated plot
  p <- p + do.call(ggplot2::geom_smooth, smooth_args)
}

#' Builds a base scatter plot for model evaluation (simulated vs observed or
#' residuals vs observed).
#'
#' Filters out missing values, draws a reference line (1:1 for simulated,
#' horizontal at 0 for residuals), optionally overlays observation error bars
#' (±2 SD), adds a regression line, facets by variable, and enforces square or
#' symmetric axes depending on the plot type.
#'
#' @param df_data Data frame containing the columns needed for the plot.
#' @param title String. Title displayed at the top of the plot.
#' @param reference_var String. Reference variable identifier passed to
#'   `give_reference_var()` to resolve the column name and display label.
#' @param select_scat String. Type of scatter plot: `"sim"` for simulated vs
#'   observed, `"res"` for residuals vs observed.
#' @param is_obs_sd Logical. Whether to draw observation error bars
#'   (±2 SD) when the x-axis is `"Observed"`.
#' @param extra_obs_sd_aes Optional named list of additional aesthetics
#'   merged into the error bar mapping. Defaults to NULL.
#' @param extra_regression_line_aes Optional named list of additional aesthetics
#'   merged into the regression line mapping. Defaults to NULL.
#'
#' @return A ggplot object representing the scatter plot.
#'
#' @keywords internal
base_scat_plot <- function(
  df_data,
  title,
  reference_var,
  select_scat,
  is_obs_sd,
  extra_obs_sd_aes = NULL,
  extra_regression_line_aes = NULL
) {
  # Resolve the reference variable column name and its display label
  tmp <- give_reference_var(reference_var)
  ref_var <- tmp$reference_var
  reference_var_name <- tmp$reference_var_name

  # Determine which y column to use based on the scatter plot type
  y_var_type <- give_y_var_type(select_scat)

  # Drop rows where either axis variable is missing
  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[ref_var]]) & !is.na(.data[[y_var_type]]))

  # Initialise the ggplot with axis mappings and situation labels
  p <- ggplot2::ggplot(
    df_data,
    ggplot2::aes(
      y = .data[[y_var_type]], x = .data[[ref_var]],
      label = .data$sit_name
    )
  ) +
    ggplot2::xlab(reference_var_name) +
    # Reference line: 1:1 for simulated, horizontal at 0 for residuals
    ggplot2::geom_abline(
      intercept = 0, slope = ifelse(select_scat == "sim", 1, 0),
      color = "grey30", linetype = 2
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(aspect.ratio = 1)

  # Overlay the linear regression line
  p <- add_regression_line(p, ref_var, y_var_type, extra_regression_line_aes)

  # Add horizontal error bars (±2 SD) when observation SD is available
  if (is_obs_sd && ref_var == "Observed") {
    final_obs_ds_aes <- ggplot2::aes(
      xmin = .data$Observed - 2 * .data$Obs_SD,
      xmax = .data$Observed + 2 * .data$Obs_SD
    )
    # Merge any provided extra aesthetics
    if (!is.null(extra_obs_sd_aes)) {
      final_obs_ds_aes <- modifyList(final_obs_ds_aes, extra_obs_sd_aes)
    }
    p <- p +
      ggplot2::geom_linerange(final_obs_ds_aes, na.rm = TRUE)
  }
  list(
    plot = p,
    data = df_data,
    x_var = ref_var,
    y_var = y_var_type
  )
}

set_facets <- function(
  p, df_data, ref_var, y_var_type, select_scat, is_obs_sd
) {
  # Facet the plot by variable with independent axis scales
  p <- add_facet_wrap(p, var = "var", scales = "free")

  # Enforce square axes (equal x/y limits) for simulated vs observed plots
  if (select_scat == "sim" && ref_var == "Observed") {
    p <- make_axis_square(df_data, ref_var, y_var_type, is_obs_sd, p)
  }

  # Force a symmetric y-axis centred on zero for residual plots
  if (select_scat == "res") {
    p <- force_y_axis(df_data, ref_var, y_var_type, is_obs_sd, p)
  }
  p
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_allsit <- function(df_data, sit, select_scat, shape_sit,
                                     reference_var, is_obs_sd, title = NULL) {
  s_plot <- base_scat_plot(
    df_data,
    title,
    reference_var,
    select_scat,
    is_obs_sd,
    extra_obs_sd_aes = ggplot2::aes(
      colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
    )
  )
  p <- s_plot$plot
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

  p <- p + ggplot2::scale_color_discrete(name = "Plant")

  set_facets(p, s_plot$data, s_plot$x_var, s_plot$y_var, select_scat, is_obs_sd)

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_mixture_versions <- function(df_data, sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  s_plot <- base_scat_plot(
    df_data,
    title,
    reference_var,
    select_scat,
    is_obs_sd,
    extra_obs_sd_aes = ggplot2::aes(colour = as.factor(.data$version)),
    extra_regression_line_aes = ggplot2::aes(colour = as.factor(.data$version))
  )

  p <- s_plot$plot

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

  set_facets(p, s_plot$data, s_plot$x_var, s_plot$y_var, select_scat, is_obs_sd)

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_allsit <- function(df_data, sit, select_scat, shape_sit,
                             reference_var, is_obs_sd, title = NULL,
                             has_distinct_situations = FALSE,
                             one_version = FALSE, mixture = FALSE) {
  extra_obs_sd_aes <- NULL
  if (shape_sit == "symbol" || shape_sit == "group") {
    extra_obs_sd_aes <- ggplot2::aes(colour = as.factor(paste(.data$sit_name)))
  }

  s_plot <- base_scat_plot(
    df_data,
    title,
    reference_var,
    select_scat,
    is_obs_sd,
    extra_obs_sd_aes = extra_obs_sd_aes
  )

  p <- s_plot$plot

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

  if (shape_sit == "txt") {
    p <- p + ggrepel::geom_text_repel(max.overlaps = 100)
  }
  if (
    has_distinct_situations == FALSE &&
      one_version == TRUE &&
      mixture == FALSE
  ) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  set_facets(p, s_plot$data, s_plot$x_var, s_plot$y_var, select_scat, is_obs_sd)

  return(p)
}

#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_per_sit <- function(df_data,
                                       sit, select_scat, shape_sit,
                                       reference_var, is_obs_sd, title = NULL) {
  s_plot <- base_scat_plot(
    df_data,
    title,
    reference_var,
    select_scat,
    is_obs_sd,
    extra_obs_sd_aes = ggplot2::aes(colour = as.factor(.data$version)),
    extra_regression_line_aes = ggplot2::aes(colour = as.factor(.data$version))
  )

  p <- s_plot$plot

  p <- p + ggplot2::geom_point(
    ggplot2::aes(colour = as.factor(.data$version)),
    na.rm = TRUE
  ) +
    ggplot2::labs(color = "Version")

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

  set_facets(p, s_plot$data, s_plot$x_var, s_plot$y_var, select_scat, is_obs_sd)

  return(p)
}


#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_versions_allsit <- function(df_data,
                                      sit, select_scat, shape_sit,
                                      reference_var, is_obs_sd, title = NULL) {
  s_plot <- base_scat_plot(
    df_data,
    title,
    reference_var,
    select_scat,
    is_obs_sd,
    extra_obs_sd_aes = ggplot2::aes(colour = as.factor(.data$version)),
    extra_regression_line_aes = ggplot2::aes(colour = as.factor(.data$version))
  )

  p <- s_plot$plot

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

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(colour = as.factor(.data$version)),
        show.legend = FALSE,
        max.overlaps = 100
      )
  }

  set_facets(p, s_plot$data, s_plot$x_var, s_plot$y_var, select_scat, is_obs_sd)

  return(p)
}

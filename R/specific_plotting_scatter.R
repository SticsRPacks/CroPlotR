
#' Specific functions to generate scatter plots
#'
#' @description Generate scatter plots for the differents cases handled in CroPlotR
#' (plant mixture, plot of residuals, plot several simulation results on same graph, ...)
#' as specitifed by the different arguments.
#'
#' @inheritParams plot_situations
#'
#' @param df_data A named list of data frame including the data to plot (one df
#' per situation, or only one df if sit==all_situations)
#' @param sit The name of the situation to plot (or all_situations)
#' @param is_obs_sd TRUE if error standard deviation of observations is provided
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
plot_scat_mixture_allsit <- function(df_data, sit, select_scat, shape_sit,
                                     reference_var, is_obs_sd, title = NULL) {

  if (is.null(reference_var)) {
    reference_var <- "Observed"
    reference_var_name <- "Observed"
  } else {
    reference_var_name <- reference_var
    reference_var <- "Reference"
  }

  if (select_scat == "sim") {
    y <- "Simulated"
    slope <- 1
  } else {
    y <- "Residuals"
    slope <- 0
  }

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y]]))

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(y = .data[[y]], x = .data[[reference_var]])
    )

  if (shape_sit == "none" | shape_sit == "txt") {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(
        colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
      ),
      na.rm = TRUE
    )
  } else if (shape_sit == "symbol" | shape_sit == "group") {
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
      intercept = 0, slope = slope, color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      method = lm, color = "blue",
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name) +
    ggplot2::labs(fill = "Plant") +
    ggplot2::facet_wrap(~variable, scales = "free")

  p <- p +
    ggplot2::ggtitle(title)

  if (is_obs_sd & reference_var == "Observed") {
    p <- p +
      ggplot2::geom_errorbarh(
        ggplot2::aes(
          xmin = .data$Observed - 2 * .data$Obs_SD,
          xmax = .data$Observed + 2 * .data$Obs_SD
        ),
        na.rm = TRUE
      )
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(
          label = .data$sit_name,
          colour = as.factor(paste(.data$Dominance, ":", .data$Plant))
        ),
        max.overlaps = 100
      )
  }

  if (select_scat == "sim" & reference_var == "Observed") {
    # Invisible points of coordinates (y,x) allowing to have both axes at
    # the same scale
    # could be done using ggh4x package ? see https://community.rstudio.com/t/plot-facet-wrap-with-free-scales-but-with-same-limits/147088/4
    p <- p + ggplot2::geom_point(ggplot2::aes(x = .data$Simulated, y = .data$Observed), alpha = 0, na.rm = TRUE)
  }

  p <- p + ggplot2::scale_color_discrete(name = "Plant")

  return(p)
}




#' @keywords internal
#' @rdname specific_scatter_plots
plot_scat_allsit <- function(df_data, sit, select_scat, shape_sit,
                                     reference_var, is_obs_sd, title = NULL) {

  if (is.null(reference_var)) {
    reference_var <- "Observed"
    reference_var_name <- "Observed"
  } else {
    reference_var_name <- reference_var
    reference_var <- "Reference"
  }

  if (select_scat == "sim") {
    y <- "Simulated"
    slope <- 1
  } else {
    y <- "Residuals"
    slope <- 0
  }

  df_data <-
    df_data %>%
    dplyr::filter(!is.na(.data[[reference_var]]) & !is.na(.data[[y]]))

  # Compute x and y axis min and max to set axis limits
  df_min <- df_data %>% group_by(variable) %>%
    summarise(across(where(is.numeric),min))
  df_max <- df_data %>% group_by(variable) %>%
    summarise(across(where(is.numeric),max))
  xaxis_min <- df_min[[reference_var]] - 0.05 * df_min[[reference_var]]
  xaxis_max <- df_max[[reference_var]] + 0.05 * df_max[[reference_var]]
  yaxis_min <- df_min[[y]] - 0.05 * df_min[[y]]
  yaxis_max <- df_max[[y]] + 0.05 * df_max[[y]]

  p <-
    ggplot2::ggplot(
      df_data,
      ggplot2::aes(y = .data[[y]], x = .data[[reference_var]])
    )

  if (shape_sit == "none" | shape_sit == "txt") {
    p <- p + ggplot2::geom_point(na.rm = TRUE)
  } else if (shape_sit == "symbol" | shape_sit == "group") {
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
      intercept = 0, slope = slope, color = "grey30", linetype = 2
    ) +
    ggplot2::geom_smooth(
      method = lm, color = "blue",
      se = FALSE, linewidth = 0.6, formula = y ~ x,
      fullrange = TRUE, na.rm = TRUE
    ) +
    ggplot2::xlab(reference_var_name)  +
    ggplot2::facet_wrap(~variable, scales = "free")

  p <- p +
    ggplot2::ggtitle(title)

  if (is_obs_sd & reference_var == "Observed") {
    p <- p +
      ggplot2::geom_errorbarh(
        ggplot2::aes(
          xmin = .data$Observed - 2 * .data$Obs_SD,
          xmax = .data$Observed + 2 * .data$Obs_SD
        ),
        na.rm = TRUE
      )

    # Update xaxis min and max following addition of error bars
    df_min <- df_data %>% mutate(barmin=Observed-2*Obs_SD) %>%
      group_by(variable) %>%
      summarise(across(where(is.numeric),min))
    df_max <- df_data %>% mutate(barmax=Observed+2*Obs_SD) %>%
      group_by(variable) %>%
      summarise(across(where(is.numeric),max))
    xaxis_min <- df_min[["barmin"]] - 0.05 * df_min[["barmin"]]
    xaxis_max <- df_max[["barmax"]] + 0.05 * df_max[["barmax"]]
  }

  p <- p + ggplot2::theme(aspect.ratio = 1)

  if (shape_sit == "txt") {
    p <- p +
      ggrepel::geom_text_repel(
        ggplot2::aes(
          label = .data$sit_name
        ),
        max.overlaps = 100
      )
  }

  # Set same limits for x and y axis for sim VS obs scatter plots
  if (select_scat == "sim" & reference_var == "Observed") {

    axis_min <- pmin(xaxis_min, yaxis_min)
    axis_max <- pmax(xaxis_max, yaxis_max)
    p <- p +
      ggh4x::facetted_pos_scales(
        x = lapply(1:length(xaxis_min), function(i) {
          ggplot2::scale_x_continuous(limits = c(axis_min[i], axis_max[i]))
        }
        ),
        y = lapply(1:length(yaxis_min), function(i) {
          ggplot2::scale_y_continuous(limits = c(axis_min[i], axis_max[i]))
        }
        )
      )

  }

  return(p)
}

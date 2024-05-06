
#' Specific functions to generate scatter plots
#'
#' @description Generate scatter plots for the differents cases handled in CroPlotR
#' (plant mixture, plot of residuals, plot several simulation results on same graph, ...)
#' as specitifed by the different arguments.
#'
#' @inheritParams generic_plotting
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
  # Different treatments for plotting sim or res
  ## TODO: should be in different functions ??? See if there are some lines of code in common

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
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::ggtitle(title)

  if (is_obs_sd) {
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
          colour = as.factor(paste(.data$Dominance, ":", .data$Plant)),
        )
      )
  }

  if (select_scat == "sim" & reference_var == "Observed") {
    # Invisible points of coordinates (y,x) allowing to have both axes at
    # the same scale
    # could be done using ggh4x package ? see https://community.rstudio.com/t/plot-facet-wrap-with-free-scales-but-with-same-limits/147088/4
    ggplot2::geom_point(ggplot2::aes(x = Simulated, y = Observed), alpha = 0, na.rm = TRUE)
  }

  p <- p + ggplot2::scale_color_discrete(name = "Plant")

  return(p)
}

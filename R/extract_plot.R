#' Extract plot(s) from ggplot
#'
#' @description Extract a plot corresponding to one or several variables from
#'  a ggplot.
#'
#' @param plot The output of plot_situations.
#' @param var Vector of variable names for which plots have to be extracted.
#' Optional, all variables considered by default.
#' @param situation A list of situations names to extract from a list of ggplots
#' @param force Continue if the plot is not possible ? E.g. no observations for
#'  scatter plots. If `TRUE`, return `NULL`, else return an error (default).
#' @param verbose Logical value for displaying information while running.
#' @param situations `r lifecycle::badge("deprecated")` `situations` is no
#'   longer supported, use `situation` instead.
#'
#' @return A (printed) list of ggplot objects, each element being a plot
#' for a situation
#'
#' @export
#'
extract_plot <- function(plot, var = NULL, situation = NULL, force = FALSE,
                         verbose = TRUE,
                         situations = lifecycle::deprecated()) {
  if (lifecycle::is_present(situations)) {
    lifecycle::deprecate_warn(
      "0.8.0", "extract_plot(situations)",
      "extract_plot(situation)"
    )
  } else {
    situations <- situation # to remove when we update inside the function
  }

  all_situations <- identical(names(plot), "all_situations")

  if (all_situations && !(is.null(situations))) {
    if (verbose) {
      cli::cli_alert_warning("{.code plot} is covering all situations")
    }
    if (force) {
      return(NULL)
    } else {
      stop("Impossible to extract situations from a list of a single ggplot covering all situations. Use `force = TRUE` to avoid this error.")
    }
  }

  if (is.null(names(plot))) {
    if (verbose) {
      cli::cli_alert_danger(
        "Please name the {.code plot} argument with the situations names."
      )
    }
    stop("plot argument is not a named list")
  } else {
    situations_names <- names(plot)
  }

  ex <- plot
  if (!is.null(var)) {
    for (name in situations_names) {
      if (!is.null(class(ex[[name]]))) {
        if (any(var %in% ex[[name]]$data$var)) {
          ex[[name]]$data <- ex[[name]]$data %>%
            dplyr::filter(.data$var %in% !!var)
        } else {
          ex[[name]] <- ggplot2::ggplot() +
            ggplot2::theme_void()
        }
      }
    }
  }
  if (!is.null(situations)) {
    situations <- match.arg(situations, names(plot), several.ok = TRUE)
    ex <- ex[situations]
  }
  ex
}

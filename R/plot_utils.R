#' Determines the number of columns for a faceted plot layout.
#'
#' Returns 1 column for a single facet, 3 columns when the number of facets is
#' a multiple of 3, and 2 columns otherwise.
#'
#' @param facets A vector of facet values whose length drives the column count.
#'
#' @return An integer: 1, 2, or 3.
#'
#' @keywords internal
get_ncol <- function(facets) {
  facetsl <- length(facets)
  if (facetsl == 1) {
    return(1)
  }
  if (facetsl %% 3 == 0) {
    return(3)
  }
  2
}

#' Adjusts the legend position of a ggplot based on label length and layout.
#'
#' Inspects the plot's scale labels to determine whether the legend fits better
#' on the right or at the bottom. Defaults to "right" and switches to "bottom"
#' when labels are long or the column count suggests a wide layout.
#'
#' Rules for placing the legend at the bottom:
#' - Any label is 40 characters or longer, or
#' - The layout has 2 columns and any label is 20 characters or longer, or
#' - The layout has 3 columns.
#'
#' @param p A ggplot object whose legend position will be adjusted.
#' @param ncol Integer. Number of columns in the faceted layout (1, 2, or 3),
#'   used to infer the available horizontal space.
#'
#' @return The input ggplot object with an updated `legend.position` theme.
#'
#' @keywords internal
adjust_legend_position <- function(p, ncol) {
  p_built <- ggplot2::ggplot_build(p)
  labels <- unlist(
    lapply(p_built$plot$scales$scales, function(s) s$get_labels())
  )
  if (length(labels) == 0) {
    return(p)
  }
  max_length <- max(nchar(labels), na.rm = TRUE)

  position <- "right"

  if (
    max_length >= 40 ||
      (ncol == 2) && max_length >= 20 ||
      (ncol == 3)
  ) {
    position <- "bottom"
  }
  p + ggplot2::theme(legend.position = position)
}

#' Adds a facet wrap to a ggplot and adjusts the legend position accordingly.
#'
#' Computes the optimal number of columns from the unique values of the faceting
#' variable, applies `facet_wrap`, then delegates legend placement to
#' `adjust_legend_position`.
#'
#' @param p A ggplot object to facet.
#' @param var String. Name of the column to facet by. Defaults to NULL.
#' @param scales String. Axis scale sharing across facets: `"free"`, `"free_x"`,
#'   `"free_y"`, or `"fixed"`. Defaults to `"free"`.
#'
#' @return The input ggplot object with facets and an adjusted legend position.
#'
#' @keywords internal
add_facet_wrap <- function(p, var = NULL, scales = "free") {
  ncol <- get_ncol(unique(p$data[[var]]))
  p <- p +
    ggplot2::facet_wrap(vars(.data[[var]]), scales = scales, ncol = ncol)
  adjust_legend_position(p, ncol)
}

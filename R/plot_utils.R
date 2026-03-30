get_ncol <- function(facets) {
  facetsl <- length(facets)
  if (facetsl == 1) {
    return(1)
  }
  ncol <- 2
  if (facetsl %% 3 == 0) {
    ncol <- 3
  }
  ncol
}

adjust_legend_position <- function(p, ncol) {
  p_built <- ggplot2::ggplot_build(p)
  labels <- unlist(
    lapply(p_built$plot$scales$scales, function(s) s$get_labels())
  )
  if (length(labels) == 0) return(p)
  max_length <- max(nchar(labels), na.rm = TRUE)

  position <- "right"

  if (
    max_length >= 40 ||
      (ncol %% 2 == 0) && max_length >= 20 ||
      (ncol %% 3 == 0)
  ) {
    position <- "bottom"
  }
  p + ggplot2::theme(legend.position = position)
}

add_facet <- function(p, var = NULL, scales = "free") {
  ncol <- get_ncol(unique(p$data[[var]]))
  p <- p +
    ggplot2::facet_wrap(vars(.data[[var]]), scales = scales, ncol = ncol)
  adjust_legend_position(p, ncol)
}
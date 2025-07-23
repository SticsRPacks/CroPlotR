#' Template aesthetics
#'
#' This function returns a template list of aesthetics for a plot, including
#' color, shape, linetype, and group.
#'
#' @return A list of aesthetics for a plot.
#'
#' @keywords internal
template_aesthetics <- function() {
  aesthetics <- list(
    "plot" = list(
      "color" = list(NULL), "shape" = list(NULL),
      "linetype" = list(NULL), "group" = list(NULL)
    ),
    "versions" = list("shape" = list(NULL), "linetype" = list(NULL))
  )
  return(aesthetics)
}


#' Detect items cases
#'
#' This function detects the cases for computing the aesthetics of a plot based on
#' whether it is a mixture or not, whether it has one or multiple versions, and
#' whether there is any overlap.
#'
#' @param is_mixture A logical value indicating whether the crop is a mixture or not.
#' @param one_version A logical value indicating whether the plot has one or multiple versions (e.g. of the model).
#' @param overlap A logical value indicating whether there is any overlapping variables in the plot.
#'
#' @return A character string indicating the case for computing the aesthetics of the plot.
#'
#' @keywords internal
detect_mixture_version_overlap <- function(is_mixture, one_version, overlap) {
  case <- switch(paste(is_mixture, !one_version, !is.null(overlap)),
    "TRUE TRUE TRUE" = "mixture_versions_overlap",
    "TRUE TRUE FALSE" = "mixture_versions_no_overlap",
    "TRUE FALSE TRUE" = "mixture_no_versions_overlap",
    "TRUE FALSE FALSE" = "mixture_no_versions_no_overlap",
    "FALSE TRUE TRUE" = "non_mixture_versions_overlap",
    "FALSE TRUE FALSE" = "non_mixture_versions_no_overlap",
    "FALSE FALSE TRUE" = "non_mixture_no_versions_overlap",
    "FALSE FALSE FALSE" = "non_mixture_no_versions_no_overlap"
  )

  return(case)
}

#' Detect items cases
#'
#' This function detects the cases for computing the aesthetics of a plot based on
#' whether it is a mixture or not, whether it has one or multiple versions, and
#' whether there are one or several situations to plot into the same plot.
#'
#' @param is_mixture A logical value indicating whether the crop is a mixture or not.
#' @param one_version A logical value indicating whether the plot has one or multiple versions (e.g. of the model).
#' @param several_sit A logical value indicating whether there are one or several situations to plot.
#'
#' @return A character string indicating the case for computing the aesthetics of the plot.
#'
#' @keywords internal
detect_mixture_version_situations <- function(is_mixture, one_version, several_sit) {
  case <- switch(paste(is_mixture, !one_version, several_sit),
    "TRUE TRUE TRUE" = "mixture_versions_situations",
    "TRUE TRUE FALSE" = "mixture_versions_no_situations",
    "TRUE FALSE TRUE" = "mixture_no_versions_situations",
    "TRUE FALSE FALSE" = "mixture_no_versions_no_situations",
    "FALSE TRUE TRUE" = "non_mixture_versions_situations",
    "FALSE TRUE FALSE" = "non_mixture_versions_no_situations",
    "FALSE FALSE TRUE" = "non_mixture_no_versions_situations",
    "FALSE FALSE FALSE" = "non_mixture_no_versions_no_situations"
  )

  return(case)
}

#' Manages the aesthetics of the graphics
#'
#' @description Manages the shape, color and line type of the graphics according
#'  to their content
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param overlap A list of lists containing the variables to represent on the
#' same graph when `type = "dynamic"`
#' @param several_sit Boolean. Must be equal to `TRUE` if sim and obs gather
#' more than one situation and if situations should be differentiated
#' on the graph.
#' @param shape_sit Shape to differentiate between situations when
#' `all_situations= TRUE`. See details.
#' @param one_version Boolean. Must be `TRUE` if several versions will be
#' plotted on the same graph.
#' @param iVersion Integer. Version number of sim
#' @param dot_args List of dot arguments given to plot function
#'
#' @details The `shape_sit` argument can be:
#' * "none" (the default): Same shape for all situations.
#' * "txt": Writes the name of the situation above each point.
#' * "symbol": One shape for each situation.
#' * "group": One shape for each group of situations described in
#' `situation_group`.
#'
#' @return An aesthetics expression which will allow to manage the aesthetics
#' of the graphics
#'
#' @keywords internal
#'
aesthetics <- function(sim, obs = NULL, type = c("dynamic", "scatter"),
                       overlap = NULL, several_sit = FALSE,
                       shape_sit = c("none", "txt", "symbol", "group"),
                       one_version = TRUE, iVersion = 1,
                       dot_args = NULL) {
  is_dominance <- grep("Dominance", x = colnames(sim), fixed = TRUE)
  if (length(is_dominance) > 0) {
    is_mixture <- length(unique(sim[[is_dominance]])) > 1
  } else {
    is_mixture <- FALSE
  }
  is_mixture <- is_mixture && (length(unique(sim$Dominance)) > 1)

  aesthetics <- template_aesthetics()

  if (type == "dynamic") {
    aesthetics <- aesthetics_dynamic(
      sim, aesthetics, overlap, one_version, iVersion, dot_args,
      is_mixture
    )
  } else if (type == "scatter") {
    aesthetics <- aesthetics_scatter(
      sim, aesthetics, overlap, several_sit, one_version, iVersion,
      dot_args, is_mixture
    )
  } else {
    stop("type must be either 'dynamic' or 'scatter'")
  }

  return(aesthetics)
}

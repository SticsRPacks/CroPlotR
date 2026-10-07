#' Detects if a situation is a mixture
#'
#' This function checks if the situation is a mixture based on
#' the presence of a column named "Dominance" and the uniqueness
#' of its values.
#'
#' @param sim_situation A data frame containing the simulated data for
#' one situation.
#' @return A logical value indicating if the situation is a mixture.
#' @examples
#' \dontrun{
#' sim_data <- data.frame(
#'   Dominance = c("Principal", "Principal", "Associated", "Associated")
#' )
#' CroPlotR:::detect_mixture(sim_data)
#' # Output: TRUE
#'
#' sim_data <- data.frame(Dominance = c("Single Crop", "Single Crop"))
#' CroPlotR:::detect_mixture(sim_data)
#' # Output: FALSE
#'
#' sim_data <- data.frame(lai = c(1, 1.2))
#' CroPlotR:::detect_mixture(sim_data)
#' # Output: FALSE
#' }
detect_mixture <- function(sim_situation) {
  is_Dominance <- grep("Dominance", x = colnames(sim_situation), fixed = TRUE)
  if (length(is_Dominance) > 0) {
    is_mixture <- length(unique(sim_situation[[is_Dominance]])) > 1
  } else {
    is_mixture <- FALSE
  }

  return(is_mixture)
}

#' Detect items cases for dynamic plots
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

#' Detect items cases for scatter plots
#'
#' This function detects the cases for computing the aesthetics of a plot based on
#' whether it is a mixture or not, whether it has one or multiple versions, and
#' whether there are one or several situations to plot into the same plot.
#'
#' @param is_mixture A logical value indicating whether the crop is a mixture or not.
#' @param one_version A logical value indicating whether the plot has one or multiple versions (e.g. of the model).
#' @param has_distinct_situations A logical value indicating whether there are one or several situations to plot.
#'
#' @return A character string indicating the case for computing the aesthetics of the plot.
#'
#' @keywords internal
detect_mixture_version_situations <- function(is_mixture, one_version, has_distinct_situations) {
  case <- switch(paste(is_mixture, !one_version, has_distinct_situations),
    "TRUE TRUE TRUE" = "mixture_versions",
    "TRUE TRUE FALSE" = "mixture_versions",
    "TRUE FALSE TRUE" = "mixture_no_versions",
    "TRUE FALSE FALSE" = "mixture_no_versions",
    "FALSE TRUE TRUE" = "non_mixture_versions_situations",
    "FALSE TRUE FALSE" = "non_mixture_versions_per_situations",
    "FALSE FALSE TRUE" = "non_mixture_no_versions_situations",
    "FALSE FALSE FALSE" = "non_mixture_no_versions_per_situations"
  )

  return(case)
}

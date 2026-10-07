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


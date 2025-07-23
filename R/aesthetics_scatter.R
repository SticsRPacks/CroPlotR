#' Manages the aesthetics of the graphics for scatter plots
#'
#' @description Manages the shape, color and line type of the graphics according
#'  to their content
#'
#' @param sim A simulation list of data.frames
#' @param aesthetics Template aesthetics list
#' @param overlap A list of lists containing the variables to represent on the
#' same graph when `type = "dynamic"`
#' @param several_sit Boolean. Must be equal to `TRUE` if sim and obs gather
#' more than one situation and if situations should be differentiated
#' on the graph.
#' @param one_version Boolean. Must be `TRUE` if several versions will be
#' plotted on the same graph.
#' @param iversion Integer. Version number of sim
#' @param dot_args List of dot arguments given to plot function
#' @param is_mixture Boolean indicating whether the crop is a mixture or not
#'
#' @return An aesthetics expression which will allow to manage the aesthetics
#' of the graphics
#'
#' @keywords internal
#'
aesthetics_scatter <- function(sim, aesthetics = template_aesthetics(),
                               overlap = NULL, several_sit = FALSE,
                               one_version = TRUE, iversion = 1,
                               dot_args = NULL, is_mixture = FALSE) {
  # Define the cases using switch
  item_case <- detect_mixture_version_situations(is_mixture, one_version, several_sit)

  # Manage the aesthetics according to the case:
  if (item_case == "mixture_no_versions_no_situations") {
    aesthetics$plot$color <-
      list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
  } else if (item_case == "non_mixture_versions_no_situations") {
    aesthetics$versions$color <- list("Versions" = names(dot_args[iversion]))
    aesthetics$versions$linetype <-
      list("Versions" = names(dot_args[iversion]))
  } else if (item_case == "non_mixture_no_versions_situations") {
    aesthetics$plot$color <- list("Situation" = quote(paste(.data$sit_name)))
  } else if (item_case == "mixture_versions_no_situations") {
    if (iversion == 1) {
      aesthetics$versions$color <-
        list("Versions" = quote(paste(names(dot_args[1]))))
      aesthetics$versions$linetype <-
        list("Versions" = quote(paste(names(dot_args[1]))))
    } else {
      aesthetics$versions$color <-
        list("Versions" = quote(paste(names(dot_args[iversion]))))
      aesthetics$versions$linetype <-
        list("Versions" = names(dot_args[iversion]))
    }
    aesthetics$plot$shape <-
      list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
  } else if (item_case == "non_mixture_versions_situations") {
    aesthetics$versions$color <- list("Versions" = names(dot_args[iversion]))
    aesthetics$versions$linetype <-
      list("Versions" = names(dot_args[iversion]))
    aesthetics$plot$shape <- list("Situation" = quote(.data$sit_name))
  } else if (item_case == "mixture_no_versions_situations") {
    aesthetics$plot$color <-
      list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
    aesthetics$plot$shape <- list("Situation" = quote(.data$sit_name))
  } else if (item_case == "mixture_versions_situations") {
    aesthetics$versions$color <- list(quote(paste(.data$Combi)))
    aesthetics$versions$linetype <-
      list("Versions" = names(dot_args[iversion]))
  }
  return(aesthetics)
}

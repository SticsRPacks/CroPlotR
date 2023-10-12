#' Manages the aesthetics of the graphics for dynamic plots
#'
#' @description Manages the shape, color and line type of the graphics according
#'  to their content
#'
#' @param sim A simulation list of data.frames
#' @param aesthetics Template aesthetics list
#' @param overlap A list of lists containing the variables to represent on the
#' same graph when `type = "dynamic"`
#' @param one_version Boolean. Must be `TRUE` if several versions will be
#' plotted on the same graph.
#' @param iVersion Integer. Version number of sim
#' @param dot_args List of dot arguments given to plot function
#' @param is_mixture Boolean indicating whether the crop is a mixture or not
#'
#' @return An aesthetics expression which will allow to manage the aesthetics
#' of the graphics
#'
#' @keywords internal
#'
aesthetics_dynamic <- function(sim, aesthetics = template_aesthetics(),
                               overlap = NULL, one_version = TRUE,
                               iVersion = 1, dot_args = NULL,
                               is_mixture = FALSE) {
    # Define the cases using switch
    item_case <- detect_mixture_version_overlap(is_mixture, one_version, overlap)

    # Manage the aesthetics according to the case:
    if (item_case == "mixture_no_versions_no_overlap") {
        aesthetics$plot$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$shape <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
    } else if (item_case == "non_mixture_versions_no_overlap") {
        aesthetics$versions$color <-
            list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$shape <- list("Versions" = names(dot_args[iVersion]))
    } else if (item_case == "non_mixture_no_versions_overlap") {
        aesthetics$plot$color <- list("Variable" = quote(.data$variable))
        aesthetics$plot$shape <- list("Variable" = quote(.data$variable))
        aesthetics$plot$group <- list("Variable" = quote(.data$variable))
    } else if (item_case == "mixture_versions_no_overlap") {
        aesthetics$versions$linetype <-
            list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$shape <-
            list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
    } else if (item_case == "non_mixture_versions_overlap") {
        aesthetics$versions$color <- list("Variable" = quote(.data$variable))
        aesthetics$versions$linetype <-
            list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$shape <- list("Versions" = names(dot_args[iVersion]))
        aesthetics$plot$color <- list("Variable" = quote(.data$variable))
        aesthetics$plot$group <- list("Variable" = quote(.data$variable))
    } else if (item_case == "mixture_no_versions_overlap") {
        aesthetics$plot$linetype <- list("Variable" = quote(.data$variable))
        aesthetics$plot$shape <- list("Variable" = quote(.data$variable))
        aesthetics$plot$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <- NULL # ! do we need this?
    } else if (item_case == "mixture_versions_overlap") {
        aesthetics$versions$color <- list(quote(paste(.data$Combi)))
        aesthetics$versions$shape <- list(quote(paste(.data$Combi)))
        aesthetics$versions$linetype <- list(quote(paste(.data$Combi)))
        aesthetics$plot$group <- list(quote(paste(.data$Combi)))
    }

    return(aesthetics)
}

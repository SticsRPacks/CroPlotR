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
    # Case where there is only one item to take into account
    # Only mixture
    if (is_mixture && one_version && is.null(overlap)) {
        aesthetics$plot$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$shape <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        # Only versions:
    } else if (!is_mixture && !one_version && is.null(overlap)) {
        if (iVersion == 1) {
            aesthetics$versions$color <-
                list("Versions" = quote(paste(names(dot_args[1]))))
            aesthetics$versions$shape <-
                list("Versions" = quote(paste(names(dot_args[1]))))
        } else {
            aesthetics$versions$color <-
                list("Versions" = names(dot_args[iVersion]))
            aesthetics$versions$shape <- list("Versions" = names(dot_args[iVersion]))
        }
        # Only overlap:
    } else if (!is_mixture && one_version && !is.null(overlap)) {
        aesthetics$plot$color <- list("Variable" = quote(.data$variable))
        aesthetics$plot$shape <- list("Variable" = quote(.data$variable))
        aesthetics$plot$group <- list("Variable" = quote(.data$variable))
    }

    # Case where there are two items to take into account

    # Mixture + versions:
    if (is_mixture && !one_version && is.null(overlap)) {
        if (iVersion == 1) {
            aesthetics$versions$linetype <-
                list("Versions" = quote(paste(names(dot_args[1]))))
            aesthetics$versions$shape <-
                list("Versions" = quote(paste(names(dot_args[1]))))
        } else {
            aesthetics$versions$linetype <-
                list("Versions" = names(dot_args[iVersion]))
            aesthetics$versions$shape <-
                list("Versions" = names(dot_args[iVersion]))
        }
        aesthetics$versions$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        # Version + overlap:
    } else if (!is_mixture && !one_version && !is.null(overlap)) {
        if (iVersion == 1) {
            aesthetics$versions$color <- list("Variable" = quote(.data$variable))
            aesthetics$versions$linetype <-
                list("Versions" = quote(paste(names(dot_args[1]))))
            aesthetics$versions$shape <-
                list("Versions" = quote(paste(names(dot_args[1]))))
        } else {
            aesthetics$versions$color <- list("Variable" = quote(.data$variable))
            aesthetics$versions$linetype <-
                list("Versions" = names(dot_args[iVersion]))
            aesthetics$versions$shape <- list("Versions" = names(dot_args[iVersion]))
        }
        aesthetics$plot$color <- list("Variable" = quote(.data$variable))
        aesthetics$plot$group <- list("Variable" = quote(.data$variable))
        # Mixture + overlap:
    } else if (is_mixture && one_version && !is.null(overlap)) {
        aesthetics$plot$linetype <- list("Variable" = quote(.data$variable))
        aesthetics$plot$shape <- list("Variable" = quote(.data$variable))
        aesthetics$plot$color <-
            list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
        aesthetics$plot$group <- NULL # ! do we need this?
    }

    # Case where there are three items to take into account
    if (is_mixture && !one_version && !is.null(overlap)) {
        aesthetics$versions$color <- list(quote(paste(.data$Combi)))
        aesthetics$versions$shape <- list(quote(paste(.data$Combi)))
        aesthetics$versions$linetype <- list(quote(paste(.data$Combi)))
        aesthetics$plot$group <- list(quote(paste(.data$Combi)))
    }

    return(aesthetics)
}

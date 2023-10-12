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
  is_Dominance <- grep("Dominance", x = colnames(sim), fixed = TRUE)
  if (length(is_Dominance) > 0) {
    is_mixture <- length(unique(sim[[is_Dominance]])) > 1
  } else {
    is_mixture <- FALSE
  }
  is_mixture <- is_mixture && (length(unique(sim$Dominance)) > 1)

  aesthetics <- list(
    "plot" = list(
      "color" = list(NULL), "shape" = list(NULL),
      "linetype" = list(NULL), "group" = list(NULL)
    ),
    "versions" = list("shape" = list(NULL), "linetype" = list(NULL))
  )


  # Case where there is only one item to take into account
  if (type == "dynamic") {
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

        if (!is.null(dot_args)) {
          # ! When plotting `plot(sim, sim)`, we get to this stage,
          # ! but `dot_args` is NULL, so then we get an error when building
          # ! the `ggplot` object. Check why we need this correction here,
          # ! we couldn't find out (S. Buis & R. Vezy)
          aesthetics$plot$group <-
            list("Versions" = quote(paste(names(dot_args[1]))))
        }
      } else {
        aesthetics$versions$color <-
          list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$shape <- list("Versions" = names(dot_args[iVersion]))
        aesthetics$plot$group <-
          list("Versions" = names(dot_args[iVersion]))
      }
      # Only overlap:
    } else if (!is_mixture && one_version && !is.null(overlap)) {
      aesthetics$plot$color <- list("Variable" = quote(.data$variable))
      aesthetics$plot$shape <- list("Variable" = quote(.data$variable))
      aesthetics$plot$group <- list("Variable" = quote(.data$variable))
    }
  }
  if (type == "scatter") {
    # Mixture && (one plot per situation or shape is a text)
    if (is_mixture && one_version && !several_sit) {
      aesthetics$plot$color <-
        list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
      # Only several versions:
    } else if (!is_mixture && !one_version && !several_sit) {
      if (iVersion == 1) {
        aesthetics$versions$color <-
          list("Versions" = quote(paste(names(dot_args[1]))))
        aesthetics$versions$linetype <-
          list("Versions" = quote(paste(names(dot_args[1]))))
      } else {
        aesthetics$versions$color <- list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$linetype <-
          list("Versions" = names(dot_args[iVersion]))
      }
      # One plot for all situations (or successive) and shape is symbol or group
    } else if (!is_mixture && one_version && several_sit) {
      aesthetics$plot$color <- list("Situation" = quote(paste(.data$Sit_Name)))
    }
  }


  # Case where there are two items to take into account
  if (type == "dynamic") {
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
  }
  if (type == "scatter") {
    # Mixture && (one plot per situation or shape is a text):
    if (is_mixture && !one_version && !several_sit) {
      if (iVersion == 1) {
        aesthetics$versions$color <-
          list("Versions" = quote(paste(names(dot_args[1]))))
        aesthetics$versions$linetype <-
          list("Versions" = quote(paste(names(dot_args[1]))))
      } else {
        aesthetics$versions$color <-
          list("Versions" = quote(paste(names(dot_args[iVersion]))))
        aesthetics$versions$linetype <-
          list("Versions" = names(dot_args[iVersion]))
      }
      aesthetics$plot$shape <-
        list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
      # Several versions and one plot for all situation (or successive) and shape is symbol or group:
    } else if (!is_mixture && !one_version && several_sit) {
      if (iVersion == 1) {
        aesthetics$versions$color <-
          list("Versions" = quote(paste(names(dot_args[1]))))
        aesthetics$versions$linetype <-
          list("Versions" = quote(paste(names(dot_args[1]))))
      } else {
        aesthetics$versions$color <- list("Versions" = names(dot_args[iVersion]))
        aesthetics$versions$linetype <-
          list("Versions" = names(dot_args[iVersion]))
      }
      aesthetics$plot$shape <- list("Situation" = quote(.data$Sit_Name))
      # Mixture and one plot for all situations (or successive) and shape is symbol or group:
    } else if (is_mixture && one_version && several_sit) {
      aesthetics$plot$color <-
        list("Plant" = quote(paste(.data$Dominance, ":", .data$Plant)))
      aesthetics$plot$shape <- list("Situation" = quote(.data$Sit_Name))
    }
  }


  # Case where there are three items to take into account
  if (is_mixture && !one_version && !is.null(overlap) && type == "dynamic") {
    aesthetics$versions$color <- list(quote(paste(.data$Combi)))
    aesthetics$versions$shape <- list(quote(paste(.data$Combi)))
    aesthetics$versions$linetype <- list(quote(paste(.data$Combi)))
    aesthetics$plot$group <- list(quote(paste(.data$Combi)))
  }
  # mixture + several versions + one plot for all situations (or successive) and shape is symbol or group:
  if (is_mixture && !one_version && several_sit && type == "scatter") {
    aesthetics$versions$color <- list(quote(paste(.data$Combi)))
    if (iVersion == 1) {
      aesthetics$versions$linetype <-
        list("Versions" = quote(paste(names(dot_args[1]))))
    } else {
      aesthetics$versions$linetype <-
        list("Versions" = names(dot_args[iVersion]))
    }
  }

  return(aesthetics)
}

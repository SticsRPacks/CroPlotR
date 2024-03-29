#' Generic formatting
#'
#' This function performs generic formatting on a data frame based on the input
#' parameters.
#'
#' @param df A data frame to be formatted (e.g. output from `format_cropr`).
#' @param var A character string indicating the variable to be formatted.
#' @param overlap A logical value indicating whether there is any overlapping
#' variables desired in the plot.
#' @param situation_group A character string indicating the situation group.
#' @param type A character string indicating the type of plot (dynamic or
#' scatter).
#' @param shape_sit A logical value indicating whether the shape should be
#' based on the situation.
#' @param several_sit A logical value indicating whether there are several
#' situations to plot.
#' @param total_vers An integer indicating the total number of versions.
#' @param num_vers An integer indicating the id of the current version.
#'
#' @return A formatted data frame.
#' @keywords internal
#'
generic_formatting <- function(df, var, overlap, situation_group, type, shape_sit, several_sit, total_vers, num_vers) {
    # Filter selected variables
    if (!is.null(var)) {
        var <- unique(c(var, subst_parenth(var)))
        var_exist <- var %in% unique(df$variable)
        if (!all(var_exist))
            stop("Unknown variable(s) in input data.frame: ",
                 paste(var[!var_exist], collapse = ", "))
        df <- df %>% dplyr::filter(.data$variable %in% var)
    }

    # Replace NAs with "Single-crop" in Dominance in order to make
    # the legend understandable
    if ("Dominance" %in% colnames(df)) {
        levels(df$Dominance) <- c("Principal", "Associated", "Single crop")
        df$Dominance[which(is.na(df$Dominance))] <- "Single crop"
    }

    # Add group_var column to data frame if overlap != null
    if (!is.null(overlap)) {
        df <- dplyr::bind_cols(
            df,
            data.frame("group_var" = rep(NA, nrow(df)))
        )
        for (vars in overlap) {
            vars <- unique(c(vars, subst_parenth(vars)))
            df$group_var[which(df$variable %in% vars)] <-
                paste(intersect(df$variable, vars), collapse = " | ")
        }
        df$group_var[which(is.na(df$group_var))] <-
            as.character(df$variable[which(is.na(df$group_var))])
    }

    # Change Sit_Name column with names of situation
    # groups if shape_sit=="group"
    if (several_sit && shape_sit == "group" && !is.null(situation_group)) {
        for (grp in seq_along(situation_group)) {
            sits <- situation_group[[grp]]
            if (!is.null(names(situation_group))) {
                df$Sit_Name[which(df$Sit_Name %in% sits)] <-
                    names(situation_group)[[grp]]
            } else {
                df$Sit_Name[which(df$Sit_Name %in% sits)] <-
                    paste(sits, collapse = ";")
            }
        }
    }

    # Add combination column if there are three different characteristics
    if (type == "dynamic" && !is.null(overlap) && (total_vers > 1) &&
        ("Plant" %in% colnames(df))) {
        df <-
            dplyr::bind_cols(
                df,
                data.frame(
                    "Combi" =
                        paste(
                            rep(paste0("Version_", num_vers), nrow(df)),
                            "|", df$variable, "|",
                            paste(df$Dominance, ":", df$Plant)
                        )
                )
            )
    }
    # NB: several_sit means one plot for all situation (or successive) and shape
    # is symbol or group
    if (type == "scatter" && several_sit && (total_vers > 1) &&
        ("Plant" %in% colnames(df))) {
        df <-
            dplyr::bind_cols(
                df,
                data.frame(
                    "Combi" =
                        paste(
                            rep(paste0("Version_", num_vers), nrow(df)),
                            "|", df$Sit_Name, "|",
                            paste(df$Dominance, ":", df$Plant)
                        )
                )
            )
    }

    return(df)
}

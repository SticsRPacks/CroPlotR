#' Generic formatting
#'
#' This function performs generic formatting on a data frame based on the input
#' parameters.
#'
#' @param df A data frame to be formatted (e.g. output from `format_cropr`).
#' @param overlap A logical value indicating whether there is any overlapping
#' variables desired in the plot.
#' @param situation_group A character string indicating the situation group.
#' @param type A character string indicating the type of plot (dynamic or
#' scatter).
#' @param shape_sit A logical value indicating whether the shape should be
#' based on the situation.
#' @param has_distinct_situations A logical value indicating whether there
#' are several
#' situations to plot.
#' @param total_vers An integer indicating the total number of versions.
#'
#' @return A long data frame with the formatted data, with columns
#' Date, Plant, Dominance, sit_name, version, variable, Simulated, Observed.
#' Column "Combi" can also be added if there are three different
#' characteristics to plot.
#' @keywords internal
#'
generic_formatting <- function(
    df,
    overlap,
    situation_group,
    type,
    shape_sit,
    has_distinct_situations,
    total_vers) {
  # Replace NAs with "Single-crop" in Dominance in order to make
  # the legend understandable
  if ("Dominance" %in% colnames(df)) {
    levels(df$Dominance) <- c("Principal", "Associated", "Single crop")
    df$Dominance[which(is.na(df$Dominance))] <- "Single crop"
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

  # Change sit_name column with names of situation
  # groups if shape_sit=="group"
  if (has_distinct_situations && shape_sit == "group" &&
    !is.null(situation_group)) {
    for (grp in seq_along(situation_group)) {
      sits <- situation_group[[grp]]
      if (!is.null(names(situation_group))) {
        df$sit_name[which(df$sit_name %in% sits)] <-
          names(situation_group)[[grp]]
      } else {
        df$sit_name[which(df$sit_name %in% sits)] <-
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
              df$version,
              "|", df$variable, "|",
              paste(df$Dominance, ":", df$Plant)
            )
        )
      )
  }
  # NB: has_distinct_situations means one plot for all situation (or
  # successive) and shape is symbol or group
  if (type == "scatter" && has_distinct_situations && (total_vers > 1) &&
    ("Plant" %in% colnames(df))) {
    df <-
      dplyr::bind_cols(
        df,
        data.frame(
          "Combi" =
            paste(
              df$version,
              "|", df$sit_name, "|",
              paste(df$Dominance, ":", df$Plant)
            )
        )
      )
  }

  # Rename variable to var
  df <- dplyr::rename(df, var = variable)

  df
}

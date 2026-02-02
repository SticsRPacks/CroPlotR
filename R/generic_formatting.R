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
#' @import data.table
#'
generic_formatting <- function(
  df,
  overlap,
  situation_group,
  type,
  shape_sit,
  has_distinct_situations,
  total_vers
) {
  # Replace NAs with "Single-crop" in Dominance in order to make
  # the legend understandable
  if ("Dominance" %chin% names(df)) {
    levels(df$Dominance) <- c("Principal", "Associated", "Single crop")
    df[is.na(Dominance), Dominance := "Single crop"]
  }

  # Add group_var column to data frame if overlap != null
  if (!is.null(overlap)) {
    df[, group_var := NA_character_]
    for (vars in overlap) {
      vars <- unique(c(vars, subst_parenth(vars)))
      lab <- paste(vars, collapse = " | ")
      df[variable %in% vars, group_var := lab]
    }
    df[is.na(group_var), group_var := as.character(variable)]
  }

  # Change sit_name column with names of situation
  # groups if shape_sit=="group"
  if (has_distinct_situations &&
        shape_sit == "group" &&
        !is.null(situation_group)) {

    for (grp in seq_along(situation_group)) {
      sits <- situation_group[[grp]]

      new_name <- if (!is.null(names(situation_group))) {
        names(situation_group)[grp]
      } else {
        paste(sits, collapse = ";")
      }

      df[sit_name %in% sits, sit_name := new_name]
    }
  }

  # Add combination column if there are three different characteristics
  if (type == "dynamic" &&
        !is.null(overlap) &&
        total_vers > 1 &&
        "Plant" %in% names(df)) {

    df[, Combi := paste(
      version,
      "|", variable, "|",
      paste(Dominance, ":", Plant)
    )]
  }
  # NB: has_distinct_situations means one plot for all situation (or
  # successive) and shape is symbol or group
  if (type == "scatter" &&
      has_distinct_situations &&
      total_vers > 1 &&
      "Plant" %in% names(df)) {

    df[, Combi := paste(
      version,
      "|", sit_name, "|",
      paste(Dominance, ":", Plant)
    )]
  }

  # Rename variable to var
  data.table::setnames(df, "variable", "var")
  df
}

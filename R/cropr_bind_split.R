#' Bind rows for class Cropr
#'
#' @description Bind simulated data of different situations
#'
#' @param sim  Simulation outputs in Cropr format (named list of `data.frame` for each situation,
#' having the attribute cropr_simulation)
#'
#' @return A single data.frame or tibble binding the rows of all data.Frames or tibbles included in sim
#'
#' @seealso split_df2sim
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#'
#' bind_rows_sim(sim)
#' }
bind_rows_sim <- function(sim){
  attr(sim,"class")=NULL   # dplyr::bind_rows does not run correctly if attribute is not NULL or if class is a vector of classes ...
  return(bind_rows(sim, .id="id") %>% rename(situation=id))
}


#' Split data.frame into Cropr format
#'
#' @description Split a row-binded data.frame (or tibble) into a Cropr format simulation list.
#'
#' @param df  A single data.frame or tibble containing simulation results (as created by `bind_rows_sim`).
#' MUST include `Date`and `situation` columns.
#'
#' @param add_cropr_attr A logical to indicate if the cropr_simulation attribute must be added to the resulting variable
#' Set FALSE if you apply the function to observed data, TRUE otherwise (optional, default value = TRUE).
#'
#' @return A named list of `data.frame` for each situation, having the attribute cropr_simulation.
#'
#' @seealso bind_rows_sim
#'
#' @importFrom tidyselect vars_select_helpers
#'
#' @export
#'
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#'
#' df <- bind_rows_sim(sim)
#' split_df2sim(df)
#' }
split_df2sim <- function(df, add_cropr_attr=TRUE){
  sim <- split(df,f=df$situation, drop = TRUE, lex.order=TRUE)
  sim <- sim[unique(df$situation)] # reorder the list as the original one

  # remove columns full of NA
  sim <-
    lapply(sim,function(y) y %>%
             select(tidyselect::vars_select_helpers$where(function(x) !all(is.na(x))))%>%
             select(-"situation") %>% remove_rownames())

  if (add_cropr_attr) {
    attr(sim, "class")= "cropr_simulation"
  }

  return(sim)
}

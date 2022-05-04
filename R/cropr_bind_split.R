#' Bind simulation list into dataframe
#'
#' @description Bind simulations list with different situations into a single dataframe
#'
#' @param ... Simulation outputs in Cropr format, *i.e.* a named list of `data.frame`
#' for each situation.
#' @param .id Name of the column in the new dataframe that identifies the origin
#' of each row. If `...` is a simulation output, it is set to "situation" by
#' default.
#'
#' @return A single data.frame or tibble binding the rows of all data.Frames or tibbles included in sim
#'
#' @details If `...` is not of class `cropr_simulation`, it uses the regular function
#' from `dplyr`. See *e.g.* [`SticsRFiles::get_sim()`] for an example output format.
#'
#' @note You can perform the same for observations with the following:
#' `bind_rows(obs, .id = "situation")`.
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
#' sim= SticsRFiles::get_sim(workspace = workspace, usm = situations)
#'
#' bind_rows(sim)
#' }
bind_rows <- function(..., .id = NULL){
  dots <- list(...)
  if (inherits(dots[[1]], "cropr_simulation")){
    if(is.null(.id)){
      .id = "situation"
    }
    dplyr::bind_rows(as.list(...), .id = .id)
  }else{
    dplyr::bind_rows(..., .id = .id)
  }
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
#' @seealso bind_rows
#'
#' @importFrom tidyselect vars_select_helpers
#'
#' @export
#'
#' @import dplyr
#' @import tibble
#' @importFrom vctrs new_list_of
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_sim(workspace = workspace, usm = situations)
#'
#' df <- bind_rows(sim)
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
    sim <- vctrs::new_list_of(sim, class = "cropr_simulation")
  }

  return(sim)
}

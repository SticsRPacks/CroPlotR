#' Summary statistics of simulations
#'
#' @description Summary statistics for one or several situations with observations, eventually grouped
#' by a model version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#'
#' @return A list of statistics `data.frame`s named by situation
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here (see [statistics()] and [format_stics()] for more information).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "STICS"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#'
#' summary(sim,obs=obs)
#' }
summary.stics_simulation= function(...,obs){
  statistics_situations(..., obs=obs, formater= format_stics)
}



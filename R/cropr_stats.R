#' Summary statistics of simulations
#'
#' @description Summary statistics for one or several situations with observations, eventually grouped
#' by a model version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param stat A character vector of required statistics, "all" for all, or any of [predictor_assessment()].
#' @param all_situations Boolean (default = TRUE). If `TRUE`, computes statistics for all situations.
#' @param verbose Boolean. Print informations during execution.
#'
#' @return A list of statistics `data.frame`s named by situation
#'
#' @seealso All the functions used to compute the statistics: [predictor_assessment()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_sim(workspace = workspace, usm = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm = situations)
#'
#' # All stats for the simulation:
#' summary(sim, obs= obs)
#'
#' # All stats for two groups of simulations:
#' summary(sim1= sim, sim2= sim, obs=obs)
#'
#' # Only R2 and nRMSE for one group:
#' summary(sim, obs=obs, stat= c("R2","nRMSE"))
#'
#' }
summary.cropr_simulation= function(...,obs,stat="all",all_situations=TRUE,verbose=TRUE){
  statistics_situations(..., obs=obs,stat=stat,all_situations=all_situations,verbose=verbose,formater= format_cropr)
}



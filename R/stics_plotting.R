#' Plot situations by group of simulation
#'
#' @description simulation outputs for one or several situations with or without observations, eventually grouped
#' by a model version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable in Y) or scatter (simulated VS observed)
#' @param plot Which data to plot in priority when `type= "dynamic"`? See details.
#' @param title A vector of plot titles, named by situation. Use the situation name if `NULL`, recycled if length one.
#' @param all_situations Boolean. If `TRUE`, plot all situations on the same graph.
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print informations during execution.
#'
#' @details The `plot` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common (used when `type= "scatter"` )
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @note The plots titles are given by their situation name.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for a situation
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#'
#' plot(sim,obs=obs)
#' }
plot.stics_simulation <- function(...,obs=NULL,type=c("dynamic","scatter"),
                                  plot=c("sim","common","obs","res","all"),title=NULL,
                                  all_situations=TRUE,force= TRUE,verbose=TRUE){
  plot_situations(..., obs=obs,type=type,plot=plot,title=title,
                  all_situations=all_situations,force= force,
                  verbose=verbose,formater= format_stics)
}

#' @rdname plot.stics_simulation
autoplot.stics_simulation <- function(...,obs=NULL,type=c("dynamic","scatter"),
                                      plot=c("sim","common","obs","res","all"),title=NULL,
                                      all_situations=TRUE,force= TRUE,verbose= TRUE) {
  plot_situations(..., obs=obs,type=type,plot=plot,title=title,
                  all_situations=all_situations,force= force,
                  verbose=verbose,formater= format_stics)
}


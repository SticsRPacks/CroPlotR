#' Extract plot(s) from ggplot
#'
#' @description Extract a plot corresponding to one or several variables from a ggplot.
#'
#' @param plot The output of plot_situations
#' @param var A list of variables to extract
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for a situation
#'
#' @export
#'
extract_plot= function(plot,var=NULL,verbose=TRUE){
  all_situations= names(plot)==list("all_situations")

  if(is.null(names(plot))){
    if(verbose){
      cli::cli_alert_danger("Please name the {.code plot} argument with the situations names.")
    }
    stop("plot argument is not a named list")
  }else{
    situations_names= names(plot)
  }

  if(is.null(var)){
    warning("No variables to extract from the plot")
  }

  ex = plot
  for(name in situations_names){
    ex[[name]]$data = ex[[name]]$data %>% dplyr::filter(variable==var)
  }
  ex
}

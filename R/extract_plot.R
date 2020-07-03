#' Extract plot(s) from ggplot
#'
#' @description Extract a plot corresponding to one or several variables from a ggplot.
#'
#' @param plot The output of plot_situations.
#' @param var A list of variables to extract.
#' @param situations A list of situations names to extract from a list of ggplots.
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for a situation
#'
#' @export
#'
extract_plot= function(plot,var=NULL,situations=NULL,force=TRUE,verbose=TRUE){ # add situations parameter
  all_situations= identical(names(plot),"all_situations")

  if(all_situations && !(is.null(situations))){
    if(verbose){
      cli::cli_alert_warning("{.code plot} is covering all situations")
    }
    if(force){
      return(NULL)
    }else{
      stop("Impossible to extract situations from a list of a single ggplot covering all situations")
    }
  }

  if(is.null(names(plot))){
    if(verbose){
      cli::cli_alert_danger("Please name the {.code plot} argument with the situations names.")
    }
    stop("plot argument is not a named list")
  }else{
    situations_names= names(plot)
  }

  ex = plot
  if(!is.null(var)){
    var=match.arg(var,unique(plot[[names(plot)[1]]]$data$variable),several.ok=TRUE)
    for(name in situations_names){
      ex[[name]]$data = plot[[name]]$data %>% dplyr::filter(.data$variable %in% var)
    }
  }
  if(!is.null(situations)){
    situations=match.arg(situations,names(plot),several.ok=TRUE)
    ex = ex[situations]
  }
  ex
}

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
extract_plot= function(plot,var=NULL,situations=NULL,force=TRUE,verbose=TRUE){
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
    for(name in situations_names){
      if(!is.null(class(ex[[name]]))){
        if(var%in%ex[[name]]$data$variable){
          ex[[name]]$data = ex[[name]]$data %>% dplyr::filter(.data$variable %in% var)
        }else{
          ex[[name]] = ggplot2::ggplot() + ggplot2::theme_void()
        }
      }
    }
  }
  if(!is.null(situations)){
    situations=match.arg(situations,names(plot),several.ok=TRUE)
    ex = ex[situations]
  }
  ex
}

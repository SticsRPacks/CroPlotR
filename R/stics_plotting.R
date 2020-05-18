#' Plot stics outputs
#'
#' @description Plots stics outputs for one situation. This function can be used as a template
#' for other model's plotting functions to be used in [plot_situations()].
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param plot The priority to either simulation or observation points if missing values (see details)
#' @param Title The plot title
#'
#' @details The `plot` argument can be:
#' * "sim": all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @importFrom rlang .data
#' @importFrom dplyr "%>%"
#' @return A ggplot object
#' @keywords internal
#'
plot_stics_situation= function(sim,obs=NULL,type=c("dynamic","scatter"),plot=c("sim","common","obs","all"),Title=NULL){

  plot= match.arg(plot, c("sim","common","obs","all"), several.ok = FALSE)
  type= match.arg(type,c("dynamic","scatter"), several.ok = FALSE)

  is_obs= !is.null(obs) && nrow(obs>0)

  if(type=="scatter"){
    plot= "common"
    if(!is_obs){
      cli::cli_alert_danger("Please provide observations to the {.code obs} argument of the function for scatter plots.")
      stop("No observations found")
    }
  }

  # format_stics()

  is_Dominance= grep("Dominance",x = colnames(sim), fixed = TRUE)
  if(length(is_Dominance)>0){
    is_mixture= length(unique(sim[[is_Dominance]]))>1
  }else{
    is_mixture= FALSE
  }

  if(is_mixture&&is_obs&&is.null(obs$Plant)){
    stop("Detected intercrop from simulation, but the 'Plant' column is missing from the observations.")
  }

  # Treating Dominance as a factor if any (for plotting reasons):
  if(is_mixture&&length(unique(sim$Dominance))>1){
    sim$Dominance= factor(sim$Dominance, levels = c("Principal","Associated"))
  }

  # Adding Dominance to obs if any:
  if(is_obs&&is_mixture){
    if(is.null(obs$Dominance)){
      # Add Dominance to obs:
      corresp_table=
        sim%>%
        dplyr::group_by(.data$Dominance)%>%
        dplyr::summarise(Plant= unique(.data$Plant))
      obs= dplyr::full_join(obs, corresp_table, by= "Plant")
    }else{
      obs$Dominance= factor(obs$Dominance, levels = c("Principal","Associated"))
    }
  }

  # Only plotting common variables:
  if(is_obs&&(plot=="sim"|plot=="common")){
    # Plot all simulations, and only obs that are simulated
    obs= obs[,intersect(colnames(obs),colnames(sim))]
  }

  if(plot=="obs"|plot=="common"){
    if(is_obs){
      # Plot all observations, and only sim that are observed
      sim= sim[,intersect(colnames(sim),colnames(obs))]
    }else{
      warning('Observations not found, try to set plot="sim" to plot the simulations only')
      return()
    }
  }

  if(is_mixture&&length(unique(sim$Dominance))>1){
    rem_vars= c("ian","mo","jo","jul","cum_jul")
    melt_vars= c("Date","Plant","Dominance")
    coloring= quote(paste(.data$Dominance,":",.data$Plant))
  }else{
    rem_vars= c("ian","mo","jo","jul","cum_jul","Plant")
    melt_vars= "Date"
    coloring= NULL
  }

  # Making the data:
  df=
    sim%>%
    dplyr::select(-tidyselect::any_of(rem_vars))%>%
    reshape2::melt(id.vars= melt_vars, na.rm = TRUE , value.name = "Simulated")%>%
    dplyr::mutate(variable= as.character(.data$variable))

  if(is_obs){

    obs=
      obs%>%
      dplyr::select(-tidyselect::any_of(rem_vars))%>%
      reshape2::melt(id.vars= melt_vars, na.rm = TRUE, value.name = "Observed")

    if(type=="scatter" || plot=="obs" || plot=="common"){
      if(is.null(obs$variable)){
        # No observations for the required variables here.
        return(NULL)
      }
    }
    obs$variable= as.character(obs$variable) # to avoid factors

    df= dplyr::full_join(df,obs,c(melt_vars,"variable"))
  }

  # Plot the simulations:
  if(type=="dynamic"){
    situation_plot=
      df%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Date, color= !!coloring))+
      ggplot2::labs(color= "Plant")+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::geom_line(na.rm = TRUE)+
      ggplot2::ggtitle(Title)

    # Adding the observations if any:
    if(is_obs){
      situation_plot= situation_plot + ggplot2::geom_point(ggplot2::aes(y= .data$Observed), na.rm = TRUE)
    }

  }else{
    situation_plot=
      df%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Observed, shape= !!coloring))+
      ggplot2::labs(shape= "Plant")+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::geom_abline(intercept = 0, slope = 1, color= "grey30", linetype= 2)+
      ggplot2::geom_point(na.rm = TRUE)+
      ggplot2::ggtitle(Title)
  }

  situation_plot
}



# Implementing generic ploting functions for STICS ------------------------


#' Plot situations by group of simulation
#'
#' @description simulation outputs for one or several situations with or without observations, grouped
#' by a model version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable in Y) or scatter (simulated VS observed)
#' @param plot Which data to plot in priority when `type= "dynamic"`? See details.
#' @param Title A vector of plot titles, named by situation. Use the situation name if `NULL`, recycled if length one.
#'
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
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "STICS"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#'
#' plot(sim,obs=obs)
#' }
plot.stics_simulation <- function(...,obs=NULL,type=c("dynamic","scatter"),
                                  plot=c("sim","common","obs","all"),Title=NULL){
  plot_situations(..., obs=obs,type=type,plot=plot,Title=Title,
                  plot_function = plot_stics_situation)
}

#' @rdname plot.stics_simulation
autoplot.stics_simulation <- function(...,obs=NULL,type=c("dynamic","scatter"),
                                      plot=c("sim","common","obs","all"),Title=NULL) {
  plot_situations(..., obs=obs,type=type,plot=plot,Title=Title,
                  plot_function = plot_stics_situation)
}


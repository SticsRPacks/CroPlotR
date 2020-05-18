#' Generic ploting of a situation
#'
#' @description Plots outputs of a model (and observations) for one situation. This function is used as a generic
#' ploting function for any models. To use it with your own model, please provide a wrapper function around your model
#' to format the outputs used by this function (see [format_stics()] for a template), and then provide your custom function as
#' an argument to this one.
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param plot The priority to either simulation or observation points if missing values (see details)
#' @param Title The plot title
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here.
#'
#' @details The `plot` argument can be:
#' * "sim": all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @importFrom rlang .data
#' @return A ggplot object
#' @keywords internal
#'
plot_generic_situation= function(sim,obs=NULL,type=c("dynamic","scatter"),
                                 plot=c("sim","common","obs","all"),Title=NULL,
                                 formater){

  plot= match.arg(plot, c("sim","common","obs","all"), several.ok = FALSE)
  type= match.arg(type,c("dynamic","scatter"), several.ok = FALSE)

  is_obs= !is.null(obs) && nrow(obs>0)

  if(type=="scatter"){
    plot= "common"
  }

  if(!is_obs && (type=="scatter" || plot=="common" || plot=="obs")){
    cli::cli_alert_danger("Please provide observations to the {.code obs} argument of the function for scatter plots.")
    stop("No observations found")
  }

  formated_outputs= formater(sim, obs, plot)

  # Plot the simulations:
  if(type=="dynamic"){
    situation_plot=
      formated_outputs$df%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Date, color= !!formated_outputs$coloring[[1]]))+
      ggplot2::labs(color= names(formated_outputs$coloring))+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::geom_line(na.rm = TRUE)+
      ggplot2::ggtitle(Title)

    # Adding the observations if any:
    if(is_obs){
      situation_plot= situation_plot + ggplot2::geom_point(ggplot2::aes(y= .data$Observed), na.rm = TRUE)
    }

  }else{
    situation_plot=
      formated_outputs$df%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Observed, shape= !!formated_outputs$coloring[[1]]))+
      ggplot2::labs(shape= names(formated_outputs$coloring))+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::geom_abline(intercept = 0, slope = 1, color= "grey30", linetype= 2)+
      ggplot2::geom_point(na.rm = TRUE)+
      ggplot2::ggtitle(Title)
  }

  situation_plot
}


#' Generic plotting function for all models
#'
#' @description Plots simulation outputs for one or several situations with or without observations, eventually grouped
#' by a model version (or any group actually).
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable in Y) or scatter (simulated VS observed)
#' @param plot Which data to plot in priority when `type= "dynamic"`? See details.
#' @param Title A vector of plot titles, named by situation. Use the situation name if `NULL`, recycled if length one.
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here (see [plot_generic_situation()] and [format_stics()] for more information).
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
#' @keywords internal
plot_situations= function(...,obs=NULL,type=c("dynamic","scatter"),
                          plot=c("sim","common","obs","all"),
                          Title=NULL,formater){
  dot_args= list(...)

  type= match.arg(type, c("dynamic","scatter"), several.ok = FALSE)

  # Name the models:
  V_names= names(dot_args)
  if(is.null(V_names)|length(V_names)<length(dot_args)){
    V_names= paste0("Version_", seq_along(dot_args))
    names(dot_args)= V_names
  }

  # Don't show group in legend if only one:
  if(length(V_names)==1){
    showlegend= FALSE
  }else{
    showlegend= TRUE
  }

  if(length(dot_args)>1){
    common_situations_models=
      Reduce(function(x,y){
        intersect(names(x),names(y))
      }, dot_args)
  }else{
    common_situations_models= names(dot_args[[1]])
  }

  if(length(Title)==1){
    Title= rep(Title,length(common_situations_models))
    names(Title)= common_situations_models
  }

  if(!is.null(Title) && length(Title) != length(common_situations_models) && is.null(names(Title))){
    cli::cli_alert_danger("Situations number is different from model(s) outputs, please name the {.code Title} argument with the situations names.")
    # Situations number is different from models outputs, can't guess which title is for which situation.
    stop("Title argument is not a named list")
  }

  if(!is.null(Title) && is.null(names(Title))){
    # Title is provided by the user, is not named, but has same length than common_situations_models, so we guess it:
    names(Title)= common_situations_models
  }

  # Initialize the plot:
  general_plot=
    lapply(common_situations_models, function(x){
      sim_plot=
        plot_generic_situation(sim = dot_args[[1]][[x]], obs = obs[[x]],type = type, plot= plot,
                               Title=if(!is.null(Title)){Title}else{x}, formater = formater)
      if(!is.null(sim_plot)){
        if(type=="dynamic"){
          sim_plot$layers[[1]]=
            ggplot2::geom_line(ggplot2::aes_(linetype= names(dot_args[1])), na.rm = TRUE)

          if(showlegend){
            sim_plot= sim_plot + ggplot2::labs(linetype= "")
          }else{
            sim_plot= sim_plot + ggplot2::guides(linetype = FALSE)
          }

        }else{
          sim_plot$layers[[2]]=
            ggplot2::geom_point(ggplot2::aes_(color= names(dot_args[1])), na.rm = TRUE)

          if(showlegend){
            sim_plot= sim_plot + ggplot2::labs(color= "")
          }else{
            sim_plot= sim_plot + ggplot2::guides(color = FALSE)
          }
        }
      }
    })
  names(general_plot)= common_situations_models

  # Add all other models versions:
  for(i in seq_along(dot_args)){
    if(i == 1) next()
    for(j in seq_along(common_situations_models)){
      tmp=
        plot_generic_situation(sim = dot_args[[i]][[j]], obs = obs[[j]],type = type,
                               formater = formater)$data
      if(is.null(tmp)){
        next()
      }
      general_plot[[j]]=
        general_plot[[j]] +
        if(type=="dynamic"){
          ggplot2::geom_line(data = tmp, ggplot2::aes_(linetype= names(dot_args[i])),
                             na.rm = TRUE)
        }else{
          ggplot2::geom_point(data = tmp, ggplot2::aes_(color= names(dot_args[i])),
                              na.rm = TRUE)
        }
    }
  }

  general_plot
}

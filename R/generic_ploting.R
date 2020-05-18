#' Generic plotting function for all models
#'
#' @description simulation outputs for one or several situations with or without observations, grouped
#' by a model version (or any group actually). Please use this function as a wrapper for your
#' own model function that you can design based on [plot_stics_situation()]. Note that all arguments should be the
#' same.
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable in Y) or scatter (simulated VS observed)
#' @param plot Which data to plot in priority when `type= "dynamic"`? See details.
#' @param Title A vector of plot titles, named by situation. Use the situation name if `NULL`, recycled if length one.
#' @param plot_function The function used to make the plots. Please design your own function that plots one situation and provide
#' it as an argument of this function.
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
plot_situations= function(...,obs=NULL,type=c("dynamic","scatter"),
                          plot=c("sim","common","obs","all"),
                          Title=NULL,plot_function= plot_stics_situation){
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
        plot_function(sim = dot_args[[1]][[x]], obs = obs[[x]],type = type, plot= plot,
                      Title=if(!is.null(Title)){Title}else{x})
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
      tmp= plot_function(sim = dot_args[[i]][[j]], obs = obs[[j]],type = type)$data
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

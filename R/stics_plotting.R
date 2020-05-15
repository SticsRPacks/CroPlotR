#' Basic function for plotting
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
#' @return A ggplot object
#' @keywords internal
#'
plot_usm= function(sim,obs=NULL,type=c("dynamic","scatter"),plot=c("sim","common","obs","all"),Title=NULL){

  plot= match.arg(plot, c("sim","common","obs","all"), several.ok = FALSE)
  type= match.arg(type,c("dynamic","scatter"), several.ok = FALSE)

  is_obs= !is.null(obs) && nrow(obs>0)

  if(type=="scatter"){
    plot= "common"
    if(!is_obs){
      stop("No observations found: need observations for scatter plot")
    }
  }

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



  # Plot the simulations according to sole or intercrop:

  if(type=="dynamic"){
    usm_plot=
      sim%>%
      dplyr::select(-tidyselect::any_of(rem_vars))%>%
      reshape2::melt(id.vars= melt_vars)%>%
      ggplot2::ggplot(ggplot2::aes(x= .data$Date, y= .data$value, color= !!coloring))+
      ggplot2::labs(color= "Plant")+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free_y')+
      ggplot2::geom_line(na.rm = TRUE)+
      ggplot2::ggtitle(Title)

    # Adding the observations if any:
    if(is_obs){
      obs=
        obs%>%
        dplyr::select(-tidyselect::any_of(rem_vars))%>%
        reshape2::melt(id.vars= melt_vars)

      if(ncol(obs)>length(melt_vars)){
        # Add obs to plot only if there is something to plot:
        usm_plot= usm_plot + ggplot2::geom_point(data = obs, na.rm = TRUE)
      }
    }
  }else{
    obs=
      obs%>%
      dplyr::select(-tidyselect::any_of(rem_vars))%>%
      reshape2::melt(id.vars= melt_vars, na.rm = TRUE, value.name = "Observed")

    if(is.null(obs$variable)){
      # No observations for the required variables here.
      return(NULL)
    }else{
      obs$variable= as.character(obs$variable) # to avoid factors
    }

    sim=
      sim%>%
      dplyr::select(-tidyselect::any_of(rem_vars))%>%
      reshape2::melt(id.vars= melt_vars, na.rm = TRUE , value.name = "Simulated")%>%
      dplyr::mutate(variable= as.character(.data$variable))


    dplyr::full_join(sim,obs,c(melt_vars,"variable"))%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Observed, color= !!coloring))+
      ggplot2::labs(color= "Plant")+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::geom_abline(intercept = 0, slope = 1, color= "grey30", linetype= 2)+
      ggplot2::geom_point(na.rm = TRUE)+
      ggplot2::ggtitle(Title)
  }

  usm_plot
}




#' Plot usms
#'
#' @description simulation outputs for one or several USMs with or without observations
#'
#' @param sim  A list of simulation output `data.frame`s for each situation (named by situation)
#' @param obs  A list of observation `data.frame`s for each situation (named by situation)
#' @param type The type of plot, either dynamic (Date in X, variable in Y) or scatter (Simulated VS observed)
#' @param plot Which data to plot ? See details.
#' @param Title The plots title. Gives the situation as Title if NULL.
#'
#' @details The `plot` argument can be:
#' * "sim": all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @importFrom dplyr "%>%"
#' @return A list of ggplot, each element being a plot for a situation
#' @export
#'
plot_usms= function(sim,obs=NULL,type=c("dynamic","scatter"),plot=c("sim","common","obs","all"),Title=NULL){
  .= NULL # To avoid CRAN check note

  if(!is.list(sim)&&is.data.frame(sim)){
    sim= list(sim= sim)
  }

  if(!is.list(obs)&&is.data.frame(obs)){
    obs= list(sim= obs)
  }

  common_usms= names(sim)%in%names(obs)
  if(!all(common_usms)){
    warning("Observations not found for usms: ",names(sim)[!common_usms])
  }
  plots=
    lapply(names(sim), function(x){
      plot_usm(sim = sim[[x]], obs = obs[[x]],type = type,
               plot = plot, Title = if(!is.null(Title)){Title}else{x})%>%
        plot(.)
    })
  names(plots)= names(sim)
  invisible(plots)
}


#' Plot usms by version
#'
#' @description simulation outputs for one or several USMs with or without observations, grouped
#' by a stics version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a list of `data.frame` for each situation
#' @param obs  A list (each element= version) of list observation `data.frame`s for each situation (named by situation)
#' @param type The type of plot, either dynamic (Date in X, variable in Y) or scatter (Simulated VS observed)
#' @param plot Which data to plot ? See details.
#' @param Title The plots title. Gives the situation as Title if NULL.
#'
#' @details The `plot` argument can be:
#' * "sim": all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @return A list of ggplot, each element being a plot for a situation
#' @export
#'
plot_usms_models= function(...,obs=NULL,type=c("dynamic","scatter"),plot=c("sim","common","obs","all"),Title=NULL){
  dot_args= list(...)

  type= match.arg(type, c("dynamic","scatter"), several.ok = FALSE)

  # Name the models:
  V_names= names(dot_args)
  if(is.null(V_names)|length(V_names)<length(dot_args)){
    V_names= paste0("Version_", seq_along(dot_args))
    names(dot_args)= V_names
  }

  if(length(dot_args)>1){
    common_usms_models=
      Reduce(function(x,y){
        union(names(x),names(y))
      }, dot_args)
  }else{
    common_usms_models= names(dot_args[[1]])
  }

  # Initialize the plot:
  general_plot=
    lapply(common_usms_models, function(x){
      sim_plot=
        plot_usm(sim = dot_args[[1]][[x]], obs = obs[[x]],type = type, plot= plot,
                 Title=if(!is.null(Title)){Title}else{x})
      if(!is.null(sim_plot)){
        if(type=="dynamic"){
          sim_plot$layers[[1]]=
            ggplot2::geom_line(ggplot2::aes_(linetype= names(dot_args[1])), na.rm = TRUE)
          sim_plot= sim_plot + ggplot2::labs(lty= "Model version")
        }else{
          sim_plot$layers[[2]]=
            ggplot2::geom_point(ggplot2::aes_(shape= names(dot_args[1])), na.rm = TRUE)
          sim_plot= sim_plot + ggplot2::labs(shape= "Model version")
        }
      }
    })
  names(general_plot)= common_usms_models

  # Add all other models versions:
  for(i in seq_along(dot_args)){
    if(i == 1) next()
    for(j in seq_along(common_usms_models)){
      tmp= plot_usm(sim = dot_args[[i]][[j]], obs = obs[[j]],type = type)$data
      if(is.null(tmp)){
        next()
      }
      general_plot[[j]]=
        general_plot[[j]] +
        if(type=="dynamic"){
          ggplot2::geom_line(data = tmp, ggplot2::aes_(linetype= names(dot_args[i])),
                             na.rm = TRUE)
        }else{
          ggplot2::geom_point(data = tmp, ggplot2::aes_(shape= names(dot_args[i])),
                              na.rm = TRUE)
        }
    }
  }

  general_plot
}


#' Save usms plot
#'
#' @description Save the plots from `plot_usms_models()`
#'
#' @param plot   The plots output from `plot_usms_models()`
#' @param path   The path to the directoy where to save the plots
#' @param suffix A suffix to append to the file name
#' @param width  The plot width
#' @param height The plot heigth
#' @param units  The units for plot width and heigth in `units ("in", "cm", or "mm")`
#' @param dpi    The plot resolution.
#' @param scale  The scaling factor.
#'
#' @details The function uses [ggplot2::ggsave()] under the hood.
#' @return Save the plots to `path`, named by the situation name, and returns the plots invisibly for piping.
#' @export
#'
plot_save= function(plot, path, suffix= "", width = 17, height=12,units="cm",dpi=200,scale = 1.2){
  for(i in seq_along(plot)){
    if(is.null(plot)){
      next()
    }
    ggplot2::ggsave(filename = paste0(names(plot)[i],suffix,".png"), plot = plot[[i]],
                    path = path,  width = width,height=height,units=units,dpi=dpi,scale = scale)
  }
  invisible(plot_save)
}

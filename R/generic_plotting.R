#' Generic plotting of a situation
#'
#' @description Plots outputs of a model (and observations) for one situation. This function is used as a generic
#' plotting function for any models. To use it with your own model, please provide a wrapper function around your model
#' to format the outputs used by this function (see [format_stics()] for a template), and then provide your custom function as
#' an argument to this one.
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#' @param obs_sd  A data.frame of standard deviations of observations
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param var A vector of variables that should be displayed on the graph.
#' @param title The plot title
#' @param all_situations Boolean (default = TRUE). If `TRUE`, plot all situations on the same graph.
#' @param overlap A list of lists containing the variables to represent on the same graph
#' when `type = "dynamic"`.
#' @param successive A list of lists containing the situations to be represented as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param shape_sit Shape to differentiate between situations when `all_situations= TRUE`. See details.
#' @param situation_group A list of lists of situations to gather when `shape_sit= "group"`.
#' @param reference_var Variable selected on x-axis when type is scaétter and select_scat is res. It is possible to select
#' between observation and simulation of the reference variable. (examples : reference_var = "lai_n_obs", reference_var = "mafruit_sim")
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with observations, and simulations outputs when there are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals (observations-simulations) in Y.
#'
#' @details The `shape_sit` argument can be:
#' * "none" (the default): Same shape for all situations.
#' * "txt": Writes the name of the situation above each point.
#' * "symbol": One shape for each situation.
#' * "group": One shape for each group of situations described in `situation_group`.
#'
#' @note The error bar will be equal to 2*`obs_sd` on each side of the point to have 95% confidence.
#'
#' @importFrom rlang .data
#' @return A ggplot object
#' @keywords internal
#'
plot_generic_situation= function(sim,obs=NULL,obs_sd=NULL,type=c("dynamic","scatter"),
                                 select_dyn=c("sim","common","obs","all"),
                                 select_scat=c("sim","res"),var=var,title=NULL,
                                 all_situations=TRUE,overlap=NULL,successive=NULL,
                                 shape_sit=c("none","txt","symbol","group"),situation_group=NULL,
                                 total_vers=1,num_vers=1,reference_var=NULL,force=TRUE,verbose=TRUE,formater){

  is_obs= !is.null(obs) && nrow(obs)>0
  is_obs_sd= !is.null(obs_sd) && nrow(obs_sd)>0
  several_sit= (all_situations || !is.null(successive)) && shape_sit%in%c("symbol","group")

  formated_df= formater(sim, obs, obs_sd, type, select_dyn, select_scat,
                             all_situations, successive=successive, reference_var=reference_var)

  # Filter selected variables
  if(!is.null(var)){
    var=match.arg(var,formated_df$variable,several.ok=TRUE)
    formated_df= formated_df %>% dplyr::filter(.data$variable %in% var)
  }

  # Replace NAs with "Single-crop" in Dominance in order to make the legend understandable
  if("Dominance"%in%colnames(formated_df)){
    levels(formated_df$Dominance) = c("Principal","Associated","Single crop")
    formated_df$Dominance[which(is.na(formated_df$Dominance))]="Single crop"
  }

  # Add group_var column to data frame if overlap != null
  if(!is.null(overlap)){
    formated_df= dplyr::bind_cols(formated_df,
                                         data.frame("group_var"=rep(NA,nrow(formated_df))))
    for(vars in overlap){
      formated_df$group_var[which(formated_df$variable%in%vars)]=paste(vars,collapse=" | ")
    }
    formated_df$group_var[which(is.na(formated_df$group_var))]=
      formated_df$variable[which(is.na(formated_df$group_var))]
  }

  # Change Sit_Name column with names of situation groups if shape_sit=="group"
  if(several_sit && shape_sit=="group" && !is.null(situation_group)){
    for(grp in seq_along(situation_group)){
      sits = situation_group[[grp]]
      if(!is.null(names(situation_group))){
        formated_df$Sit_Name[which(formated_df$Sit_Name%in%sits)]=names(situation_group)[[grp]]
      }else{
        formated_df$Sit_Name[which(formated_df$Sit_Name%in%sits)]=paste(sits,collapse=";")
      }
    }
  }

  # Add combination column if there are three different characteristics
  if(type=="dynamic" && !is.null(overlap) && (total_vers>1) && ("Plant"%in%colnames(formated_df))){
    formated_df=
      dplyr::bind_cols(formated_df,
                       data.frame("Combi"=paste(rep(paste0("Version_",num_vers),nrow(formated_df)),"|",
                                                formated_df$variable,"|",
                                                paste(formated_df$Dominance,":",formated_df$Plant))))
  }
  if(type=="scatter" && several_sit && (total_vers>1) && ("Plant"%in%colnames(formated_df))){
    formated_df=
      dplyr::bind_cols(formated_df,
                       data.frame("Combi"=paste(rep(paste0("Version_",num_vers),nrow(formated_df)),"|",
                                                formated_df$Sit_Name,"|",
                                                paste(formated_df$Dominance,":",formated_df$Plant))))
  }

  # If there are successive situations, define borders to plot a vertical line between them
  if(!is.null(successive) && "Sit_Name"%in%colnames(sim)){
    borders=
      lapply(unique(sim$Sit_Name), function(x){
        sim_part= sim %>% dplyr::filter(.data$Sit_Name==x)
        sim_part$Date[nrow(sim_part)]
      })
    borders= borders[-length(borders)]
  }

  # In case obs is given but no common variables between obs and sim:
  if(is.null(formated_df$Observed)){
    is_obs= FALSE
  }

  if(is.null(formated_df) || (!is_obs && (type=="scatter" || select_dyn=="common" || select_dyn=="obs"))){
    # No common observations and simulations when type=="scatter" or select_dyn=="common" or select_dyn=="obs"
    if(verbose){
      cli::cli_alert_warning("No observations found for required variables")
    }
    if(force){
      return(NULL)
    }else{
      stop("No observations found")
    }
  }

  aesth= aesthetics(sim,obs,type=type,overlap=overlap,several_sit=several_sit,
                    shape_sit=shape_sit,one_version=(total_vers==1))$plot

  # Plot the simulations:
  if(type=="dynamic"){
    situation_plot=
      formated_df%>%
      ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Date, linetype= !!aesth$linetype[[1]],
                                   shape= !!aesth$shape[[1]], color= !!aesth$color[[1]]))+
      ggplot2::geom_line(na.rm = TRUE)+
      ggplot2::labs(color= names(aesth$color),linetype= names(aesth$linetype), shape= names(aesth$shape))+
      ggplot2::scale_shape_manual(values=c(0:40))+
      ggplot2::ggtitle(title)+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    # Facet based on variable or group_var according to overlap
    if(is.null(overlap)){
      situation_plot= situation_plot + ggplot2::facet_wrap(.~.data$variable, scales = 'free')
    }else{
      situation_plot= situation_plot +
        ggplot2::facet_wrap(.~.data$group_var, scales = 'free')+
        ggplot2::theme(strip.text.x = ggplot2::element_blank())
    }
    # Adding the observations if any:
    if(is_obs){
      situation_plot= situation_plot + ggplot2::geom_point(ggplot2::aes(y= .data$Observed), na.rm = TRUE)
      if(is_obs_sd){
        situation_plot= situation_plot +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$Observed-2*.data$Obs_SD,
                                              ymax=.data$Observed+2*.data$Obs_SD),na.rm=TRUE)
      }
    }
    # Add vertical lines if sim contains successive situations
    if(!is.null(successive) && "Sit_Name"%in%colnames(sim)){
      for(xint in borders){
        situation_plot= situation_plot + ggplot2::geom_vline(xintercept=xint,linetype="dashed",color="grey",size=1)
      }
    }

  }else{
    if(select_scat=="sim"){
      situation_plot=
        formated_df%>%
        ggplot2::ggplot(ggplot2::aes(y= .data$Simulated, x= .data$Observed, shape= !!aesth$shape[[1]],
                                     linetype = !!aesth$linetype[[1]], color= !!aesth$color[[1]]))+
        ggplot2::geom_point(na.rm=TRUE)+
        ggplot2::geom_abline(intercept = 0, slope = 1, color= "grey30", linetype= 2)+
        ggplot2::geom_smooth(ggplot2::aes(group=1),method=lm,color="blue",se=FALSE,size=0.6,formula= y ~ x,fullrange=TRUE,na.rm=TRUE)+
        # Invisible points of coordinates (y,x) allowing to have both axes at the same scale
        ggplot2::geom_point(mapping = ggplot2::aes(x = .data$Simulated, y = .data$Observed), alpha = 0, na.rm=TRUE)

      if(is_obs_sd){
        situation_plot= situation_plot +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin=.data$Observed-2*.data$Obs_SD,
                                               xmax=.data$Observed+2*.data$Obs_SD),na.rm=TRUE)
      }
    }

    if(select_scat=="res") {
      situation_plot=
        if(is.null(reference_var)){
          formated_df%>%
          ggplot2::ggplot(ggplot2::aes(y= .data$Observed - .data$Simulated, x= .data$Observed, shape= !!aesth$shape[[1]],
                                         linetype = !!aesth$linetype[[1]], color= !!aesth$color[[1]]))
        }else{
          formated_df%>%
          ggplot2::ggplot(ggplot2::aes(y= .data$Observed - .data$Simulated, x= .data$Reference, shape= !!aesth$shape[[1]],
                                         linetype = !!aesth$linetype[[1]], color= !!aesth$color[[1]]))
        }
      situation_plot= situation_plot +
        ggplot2::ylab("Residuals")+
        ggplot2::geom_point(na.rm=TRUE)+
        ggplot2::geom_abline(intercept = 0, slope = 0, color= "grey30", linetype= 2)+
        ggplot2::geom_smooth(ggplot2::aes(group=1),method=lm, se=FALSE,size=0.6, formula = y ~ x, fullrange=TRUE, na.rm=TRUE)+
        # Invisible points of coordinates (y,x) allowing to have both axes at the same scale
        if(is.null(reference_var)){
          formated_df%>%
          ggplot2::geom_point(mapping = ggplot2::aes(x = .data$Observed - .data$Simulated, y = .data$Observed), alpha = 0, na.rm=TRUE)
        }else{
          formated_df%>%
          ggplot2::geom_point(mapping = ggplot2::aes(x = .data$Observed - .data$Simulated, y = .data$Reference), alpha = 0, na.rm=TRUE)
        }
    }

    situation_plot= situation_plot+
      formated_df%>%
      ggplot2::labs(shape= names(aesth$shape),linetype=names(aesth$linetype),color=names(aesth$color))+
      ggplot2::facet_wrap(.~.data$variable, scales = 'free')+
      ggplot2::theme(aspect.ratio=1)+
      ggplot2::ggtitle(title)+
      if(shape_sit=="txt"){
        ggrepel::geom_text_repel(ggplot2::aes(label= .data$Sit_Name), na.rm = TRUE, vjust=-0.5, show.legend = FALSE)
      }

    if(all_situations){
      situation_plot= situation_plot + ggplot2::ggtitle(ggplot2::element_blank())
    }
    if(!is.null(reference_var)){
      situation_plot= situation_plot + ggplot2::xlab(reference_var)
    }

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
#' @param obs_sd  A list (each element= situation) of standard deviations of observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable in Y) or scatter (simulated VS observed)
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param var A vector of variables that should be displayed on the graph.
#' @param title A vector of plot titles, named by situation. Use the situation name if `NULL`, recycled if length one.
#' @param all_situations Boolean (default = TRUE). If `TRUE`, plot all situations on the same graph.
#' @param overlap A list of lists containing the variables to represent on the same graph
#' when `type = "dynamic"`.
#' @param successive A list of lists containing the situations to be represented as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param shape_sit Shape to differentiate between situations when `all_situations= TRUE`. See details.
#' @param situation_group A list of lists of situations to gather when `shape_sit= "group"`.
#' @param reference_var Variable selected on x-axis when type is scaétter and select_scat is res. It is possible to select
#' between observation and simulation of the reference variable. (examples : reference_var = "lai_n_obs", reference_var = "mafruit_sim")
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here (see [plot_generic_situation()] and [format_stics()] for more information).
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with observations, and simulations outputs when there are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals (observations-simulations) in Y.
#'
#' @details The `shape_sit` argument can be:
#' * "none" (the default): Same shape for all situations.
#' * "txt": Writes the name of the situation above each point.
#' * "symbol": One shape for each situation.
#' * "group": One shape for each group of situations described in `situation_group`.
#'
#' @note The plots titles are given by their situation name.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for a situation
#'
#' @keywords internal
plot_situations= function(...,obs=NULL,obs_sd=NULL,type=c("dynamic","scatter"),
                          select_dyn=c("sim","common","obs","all"),
                          select_scat=c("sim","res"),var=NULL,title=NULL,
                          all_situations=TRUE,overlap=NULL,successive=NULL,
                          shape_sit=c("none","txt","symbol","group"),situation_group=NULL,
                          reference_var=NULL,force=TRUE,verbose=TRUE,formater){
  dot_args= list(...)

  type= match.arg(type, c("dynamic","scatter"), several.ok = FALSE)
  select_dyn= match.arg(select_dyn,c("sim","common","obs","all"), several.ok = FALSE)
  select_scat= match.arg(select_scat,c("sim","res"), several.ok = FALSE)
  shape_sit= match.arg(shape_sit,c("none","txt","symbol","group"), several.ok = FALSE)


  if(select_scat=="res" || shape_sit!="none"){
    type="scatter"
  }

  # Disable all_situations when type=="dynamic" temporarily
  if(type=="dynamic"){
    all_situations= FALSE
  }

  # Enable successive and overlap only when type=="dynamic"
  if(!is.null(successive) || !is.null(overlap)){
    type="dynamic"
  }

  # Text on scatter plots only if all_situations
  if(shape_sit!="none"){
    all_situations= TRUE
  }

  # Enable reference variable edit only when plotting residual scatter plot
  if(!is.null(reference_var)){
    type= "scatter"
    select_scat= "res"
  }

  if(!is.null(situation_group)){
    shape_sit= "group"
  }

  if(shape_sit=="group" && is.null(situation_group)){
    if(verbose){
      cli::cli_alert_warning("Argument `situation_group` must be defined when `shape_sit` is 'group'")
    }
    if(force){
      return(NULL)
    }else{
      stop("Argument `situation_group` not defined")
    }
  }

  several_sit= (all_situations || !is.null(successive)) && shape_sit%in%c("symbol","group")

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

  # Cat situations that need to be represented as a contiguous sequence (dynamic)
  if(!is.null(successive)){
    list_rot= cat_successive(dot_args,obs,successive)
    dot_args= list_rot[[1]]
    obs= list_rot[[2]]
  }

  # Intersect situations between versions
  common_situations_models= names(dot_args[[1]])
  if(length(dot_args)>1){
    for(indice in 2:length(dot_args)){
      common_situations_models= intersect(common_situations_models,names(dot_args[[indice]]))
    }
  }

  # In order to plot a single graph with all situations
  if(all_situations){
    if(is.null(obs)){
      situations_names= common_situations_models
    }else{
      situations_names= intersect(common_situations_models,names(obs)) # In case a simulation or an observation is missing
    }
    common_situations_models= list("all_situations")
  }

  if(length(title)==1){
    if(!all_situations){
      title= rep(title,length(common_situations_models))
      names(title)= common_situations_models
    }else{
      title= title
    }
  }

  if(!is.null(title) && length(title) != length(common_situations_models) && is.null(names(title))){
    if(verbose){
      cli::cli_alert_danger("Situations number is different from model(s) outputs, please name the {.code title} argument with the situations names.")
    }
    # Situations number is different from models outputs, can't guess which title is for which situation.
    stop("title argument is not a named list")
  }

  if(!is.null(title) && is.null(names(title))){
    # title is provided by the user, is not named, but has same length than common_situations_models, so we guess it:
    names(title)= common_situations_models
  }

  # Restructure data into a list of one single element if all_situations
  if(all_situations){
    list_data= cat_situations(dot_args,obs,obs_sd)
    dot_args= list_data[[1]]
    obs= list_data[[2]]
    obs_sd= list_data[[3]]
  }


  # Initialize the plot:
  general_plot=
    lapply(common_situations_models, function(x){
      sim_plot=
        plot_generic_situation(sim = dot_args[[1]][[x]], obs = obs[[x]], obs_sd= obs_sd[[x]],
                               type = type, select_dyn = select_dyn, select_scat = select_scat,
                               var=var, title=if(!is.null(title)){title}else{x},
                               all_situations=all_situations,overlap=overlap,
                               successive=successive,shape_sit=shape_sit,situation_group=situation_group,
                               total_vers=length(dot_args),num_vers=1,reference_var=reference_var,
                               force=force,verbose=verbose,formater=formater)
      if(!is.null(sim_plot)){

        aesth= aesthetics(dot_args[[1]][[x]],obs[[x]],type=type,overlap=overlap,several_sit=several_sit,
                          shape_sit=shape_sit,one_version=(length(dot_args)==1))$versions

        if(type=="dynamic"){
          sim_plot$layers[[1]]=
            if(is.null(aesth$linetype[[1]])){
              ggplot2::geom_line(ggplot2::aes_(), na.rm = TRUE)
            }else{
              ggplot2::geom_line(ggplot2::aes_(color= aesth$color[[1]],linetype= aesth$linetype[[1]]), na.rm = TRUE)
            }

          if(!is.null(aesth$shape[[1]]) && !is.null(obs[[x]]) && (nrow(obs[[x]])>0)){
            sim_plot$layers[[2]]=
              ggplot2::geom_point(ggplot2::aes_(y= sim_plot$data$Observed, color= aesth$color[[1]],
                                                shape= aesth$shape[[1]]), na.rm = TRUE)
          }

          if(!is.null(obs_sd[[x]]) && (nrow(obs_sd[[x]])>0)){
            sim_plot$layers[[3]]=
              ggplot2::geom_errorbar(ggplot2::aes_(ymin=sim_plot$data$Observed-2*sim_plot$data$Obs_SD,
                                                  ymax=sim_plot$data$Observed+2*sim_plot$data$Obs_SD),
                                     width=(pmax(sim_plot$data$Date)-pmin(sim_plot$data$Date))/length(sim_plot$data$Date),na.rm=TRUE)
          }

          if(showlegend){
            sim_plot= sim_plot + ggplot2::labs("")
          }else{
            sim_plot= sim_plot
          }

        }else{
          sim_plot$layers[[1]]=
            if(is.null(aesth$color[[1]])){
              ggplot2::geom_point(ggplot2::aes_(), na.rm = TRUE)
            }else{
              ggplot2::geom_point(ggplot2::aes_(color= aesth$color[[1]]), na.rm = TRUE)
            }

          sim_plot$layers[[3]]=
            if(is.null(aesth$linetype[[1]])){
              ggplot2::geom_smooth(ggplot2::aes_(group=1),
                                   method=lm,colour="blue",se=FALSE,size=0.6,formula= y ~ x,fullrange=TRUE,na.rm=TRUE)
            }else{
              ggplot2::geom_smooth(ggplot2::aes_(linetype= aesth$linetype[[1]], group=1),
                                   method=lm,colour="blue",se=FALSE,size=0.6,formula= y ~ x,fullrange=TRUE,na.rm=TRUE)
            }

          if(shape_sit=="txt"){
            sim_plot$layers[[5]]=
              if(is.null(aesth$color[[1]])){
                ggrepel::geom_text_repel(ggplot2::aes_(label=sim_plot$data$Sit_Name), na.rm = TRUE,
                                   vjust=-0.5, show.legend = FALSE)
              }else{
                ggrepel::geom_text_repel(ggplot2::aes_(label=sim_plot$data$Sit_Name,color= aesth$color[[1]]),
                                   na.rm = TRUE, vjust=-0.5, show.legend = FALSE)
              }
          }

          if(showlegend){
            sim_plot= sim_plot + ggplot2::labs("")
          }else{
            sim_plot= sim_plot
          }
        }
      }
    })
  names(general_plot)= common_situations_models

  # Add all other models versions:
  for(i in seq_along(dot_args)){
    if(i == 1) next()
    for(j in common_situations_models){
      tmp=
        plot_generic_situation(sim = dot_args[[i]][[j]], obs = obs[[j]], obs_sd = obs_sd[[j]],
                               type = type, select_dyn = select_dyn, select_scat = select_scat,
                               var=var, all_situations=all_situations, overlap=overlap,
                               successive=successive,shape_sit=shape_sit,situation_group=situation_group,
                               total_vers=length(dot_args),num_vers=i,reference_var=reference_var,force=force,
                               verbose=verbose,formater=formater)$data

      aesth= aesthetics(dot_args[[i]][[j]],obs[[j]],type=type,overlap=overlap,several_sit=several_sit,
                        shape_sit=shape_sit,one_version=FALSE,first_sim=FALSE)$versions

      if(is.null(tmp)){
        next()
      }
      general_plot[[j]]=
        general_plot[[j]] +
        if(type=="dynamic"){
          ggplot2::geom_line(data = tmp, ggplot2::aes_(color= aesth$color[[1]], linetype= aesth$linetype[[1]]),
                             na.rm = TRUE)
        }else{
          ggplot2::geom_point(data = tmp, ggplot2::aes_(color= aesth$color[[1]]),
                              na.rm = TRUE)
        }

      # Add observations points if any when type is dynamic
      if(!is.null(obs[[j]]) && (nrow(obs[[j]])>0) && type=="dynamic"){
        general_plot[[j]]= general_plot[[j]] +
          ggplot2::geom_point(data = tmp, ggplot2::aes_(y= tmp$Observed, color= aesth$color[[1]], shape= aesth$shape[[1]]),
                              na.rm = TRUE)
        if(!is.null(obs_sd[[j]]) && (nrow(obs_sd[[j]])>0)){
          general_plot[[j]]= general_plot[[j]] +
            ggplot2::geom_errorbar(data = tmp, ggplot2::aes_(ymin=tmp$Observed-2*tmp$Obs_SD,ymax=tmp$Observed+2*tmp$Obs_SD,
                                                             color= aesth$color[[1]], linetype= aesth$linetype[[1]]),
                                   width=10,na.rm=TRUE)
        }
      }

      # Add regression line if any when type is scatter
      if(!is.null(aesth$linetype[[1]]) && type=="scatter"){
        general_plot[[j]]= general_plot[[j]] +
          ggplot2::geom_smooth(data = tmp, ggplot2::aes_(linetype= aesth$linetype[[1]], group=1),
                              method=lm,colour="blue",se=FALSE,size=0.6,formula= y ~ x,fullrange=TRUE,na.rm=TRUE)
      }

      if(shape_sit=="txt" && type=="scatter"){
        general_plot[[j]]= general_plot[[j]]+
          ggrepel::geom_text_repel(data = tmp, ggplot2::aes_(label=tmp$Sit_Name,color= aesth$color[[1]]),
                             na.rm = TRUE, vjust=-0.5, show.legend = FALSE)
      }

    }
  }

  general_plot
}


#' Plot statistics
#'
#' @param x The output of [summary.stics_simulation()]
#' @param xvar The variable to use in x, either the group or the situation (the other is used for colouring)
#' @param type The type of plot requested, either "bar" (bar plot) or "radar" (radar chart)
#' @param group_bar Way to display the different statistical criteria when `type= "bar"`. See details.
#' @param crit_radar Statistical criterion chosen to be displayed on the radar chart.
#' @param title The plot title
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#' @param ... Other arguments to pass (for backward compatibility only)
#'
#' @details The `group_bar` argument can be:
#' * "rows" (the default): One line of graphs per statistical criterion
#' * "stack": Bars of each statistical criterion stacked
#' * "dodge": Bars of each statistical criterion side by side
#'
#' @return Return a ggplot object with statistics
#'
#' @export
#'
#' @rdname plot.statistics
#'
#' @examples
#' # Importing an example with three situations with observation:
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#'
#' # R2 and nRMSE stats for the simulation:
#' stats= summary(sim, obs= obs, stat= c("R2","nRMSE"))
#' plot(stats)
#'
#' # Change the group name:
#' stats= summary("stics v9.0"= sim, obs= obs, stat= c("R2","nRMSE"))
#' plot(stats)
#'
#' # R2 and nRMSE stats for two groups of simulations:
#' summary(sim1= sim, sim2= sim, obs=obs, stat= c("R2","nRMSE"))
#'
plot.statistics <- function(x,xvar=c("group","situation"),type=c("bar","radar"),
                            group_bar=c("rows","stack","dodge"),crit_radar=NULL,
                            title=NULL,force=TRUE,verbose=TRUE,...){

  xvar= match.arg(xvar,c("group","situation"))
  type= match.arg(type,c("bar","radar"))
  group_bar= match.arg(group_bar,c("rows","stack","dodge"))

  is_one_group= length(unique(x$group))==1 # test if there is one group only
  is_all_situations= unique(unique(x$situation)=="all_situations") # test if there are all situations

  nvar= length(unique(x$variable))

  x=
    x%>%
    reshape2::melt(id.vars= c("group","situation","variable"), variable.name="statistic")

  if(type=="bar"){
    if(is.null(title)){
      title= ""
    }

    if(xvar=="group"){
      filling= quote(.data$situation)
      xvariable= quote(.data$group)
      if(group_bar=="rows"){
        showlegend= !is_all_situations
      }else{
        showlegend= length(unique(x$statistic))>1
      }
    }else{
      filling= quote(.data$group)
      xvariable= quote(.data$situation)
      if(group_bar=="rows"){
        showlegend= !is_one_group
      }else{
        showlegend= length(unique(x$statistic))>1
      }
    }

    if(group_bar=="rows"){
      x=
        x%>%
        ggplot2::ggplot(ggplot2::aes(y=.data$value, x= !!xvariable))+
        ggplot2::facet_grid(rows = ggplot2::vars(.data$statistic),
                            cols = ggplot2::vars(.data$variable),  scales = 'free')+
        ggplot2::geom_col(ggplot2::aes(fill=!!filling), position="dodge")+
        ggplot2::ggtitle(title)
    }else{
      x=
        x%>%
        ggplot2::ggplot(ggplot2::aes(y=.data$value, x= !!xvariable))+
        ggplot2::facet_grid(rows = ggplot2::vars(!!filling),
                            cols = ggplot2::vars(.data$variable),  scales = 'free')+
        ggplot2::geom_col(ggplot2::aes(fill=.data$statistic), position=group_bar)+
        ggplot2::ggtitle(title)
    }

    # Rotate variable names if too many variables
    if(nvar>8){
      x= x + ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 90))
    }

    # Rotate situation names if they are on x-axis
    if(xvar=="situation" || (group_bar=="stack" && xvar=="situation")){
      x= x + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }

    if(!showlegend){
      x= x + ggplot2::guides(fill = FALSE)
    }

    # No need to label x-axis if only one value
    if((xvar=="situation" && is_all_situations) || (xvar=="group" && is_one_group)){
      x= x + ggplot2::xlab("") + ggplot2::theme(axis.text.x=ggplot2::element_blank()) +
             ggplot2::theme(axis.ticks.x=ggplot2::element_blank())
    }

    # No need to label rows if only one
    if(group_bar!="rows" && ((xvar=="group" && is_all_situations)||
                             (xvar=="situation" && is_one_group))){
      x= x + ggplot2::theme(strip.text.y = ggplot2::element_blank())
    }

  }else{
    if(is.null(crit_radar)){
      if(verbose){
        cli::cli_alert_warning("{.code crit_radar} argument missing")
      }
      if(force){
        return(NULL)
      }else{
        stop("No statistical criteria to plot")
      }
    }

    x= x %>% dplyr::filter(.data$statistic == crit_radar)
    V_names= unique(x$group)

    x=
      x%>%
      ggplot2::ggplot(ggplot2::aes(x=.data$variable, y=.data$value, group=.data$group,
                                   colour=.data$group, fill=.data$group)) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_polygon(size = 1, alpha= 0.2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0(crit_radar)) +
      ggplot2::ggtitle(if(is.null(title)) {paste0(crit_radar)} else {title})  +
      ggplot2::scale_x_discrete() +
      ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar, theta="x", r="y", start=-pi/6,
                       direction=sign(1), is_linear = function(coord) TRUE)
  }


  x
}

#' Format a STICS simulation (and observation)
#'
#' @description Format a STICS simulation (and observation if any) for plotting. This
#' function can be used as a template to include other models in CroPlotR.
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param plot The priority to either simulation or observation points if missing values (see details)
#'
#' @details The `plot` argument can be:
#' * "sim": all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with obervations, and simulations outputs when there are some
#' * "all": all variables with any obervations or simulations outputs
#'
#' @importFrom rlang .data
#' @importFrom dplyr "%>%"
#'
#' @return A list of two: a pre-formatted `data.frame`, and a colouring expression, or `NULL` if
#' the formatting is not possible (e.g. plot="common" but no common variables in obs and sim).
#'
#' @export
#'
#' @examples
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situation= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))[1]
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situation)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situation)
#' formated_df= format_stics(sim$`IC_Wheat_Pea_2005-2006_N0`,obs$`IC_Wheat_Pea_2005-2006_N0`)
#' options(max.print= 100)
#' formated_df
format_stics= function(sim,obs=NULL,plot=c("sim","common","obs","res","all")){
  plot= match.arg(plot, c("sim","common","obs","res","all"), several.ok = FALSE)

  is_obs= !is.null(obs) && isTRUE(nrow(obs)>0)

  is_Dominance= grep("Dominance",x = colnames(sim), fixed = TRUE)
  if(length(is_Dominance)>0){
    is_mixture= length(unique(sim[[is_Dominance]]))>1
  }else{
    is_mixture= FALSE
  }

  if(is_mixture && is_obs && is.null(obs$Plant)){
    stop("Detected intercrop from simulation, but the 'Plant' column is missing from the observations.")
  }

  # Treating Dominance as a factor if any (for plotting reasons):
  if(is_mixture && length(unique(sim$Dominance))>1){
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
  if(is_obs&&(plot=="sim"||plot=="common")){
    # Plot all simulations, and only obs that are simulated
    obs= obs[,intersect(colnames(obs),colnames(sim))]
  }

  if(plot=="obs"||plot=="common"){
    if(is_obs){
      # Plot all observations, and only sim that are observed
      sim= sim[,intersect(colnames(sim),colnames(obs))]
    }else{
      return(NULL)
    }
  }

  if(is_mixture&&length(unique(sim$Dominance))>1){
    rem_vars= c("ian","mo","jo","jul","cum_jul")
    melt_vars= c("Date","Plant","Dominance")
    coloring= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
  }else{
    rem_vars= c("ian","mo","jo","jul","cum_jul","Plant")
    melt_vars= "Date"
    coloring= list("Plant"= NULL)
  }

  # Making the data:
  df=
    sim%>%
    dplyr::select(-tidyselect::any_of(rem_vars))%>%
    reshape2::melt(id.vars= melt_vars, na.rm = TRUE , value.name = "Simulated")

  if(is_obs){

    obs=
      obs%>%
      dplyr::select(-tidyselect::any_of(rem_vars))%>%
      reshape2::melt(id.vars= melt_vars, na.rm = TRUE, value.name = "Observed")

    if(plot=="obs" || plot=="common"){
      if(is.null(obs$variable)){
        # No observations for the required variables here.
        return(NULL)
      }
    }else{
      if(is.null(obs$variable)){
        # No observations for the required variables here.
        return(list(df= df, coloring= coloring))
      }
    }
    obs$variable= as.character(obs$variable) # to avoid factors

    if(is.null(df$variable)){
      # No common variables between obs and sim (case where plot=="common")
      return(list(df= obs, coloring= coloring))
    }else{
      df$variable= as.character(df$variable)
    }

    df= dplyr::full_join(df,obs,c(melt_vars,"variable"))
  }

  return(list(df= df, coloring= coloring))
}

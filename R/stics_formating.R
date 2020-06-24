#' Format a STICS simulation (and observation)
#'
#' @description Format a STICS simulation (and observation if any) for plotting. This
#' function can be used as a template to include other models in CroPlotR.
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param all_situations Boolean (default = FALSE). If `TRUE`, plot all situations on the same graph.
#' If `TRUE`, \code{sim} and \code{obs} are respectively an element of the first element and the
#' second element of the output of cat_situations.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and observations when there are some
#' * "common": variables with simulations outputs and observations in common (used when `type= "scatter"` )
#' * "obs": all variables with observations, and simulations outputs when there are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals (observations-simulations) in Y.
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
format_stics= function(sim,obs=NULL,type=c("dynamic","scatter"),
                       select_dyn=c("sim","common","obs","all"),
                       select_scat=c("sim","res"),all_situations=FALSE){

  type= match.arg(type, c("dynamic","scatter"), several.ok = FALSE)
  select_dyn= match.arg(select_dyn,c("sim","common","obs","all"), several.ok = FALSE)
  select_scat= match.arg(select_scat,c("sim","res"), several.ok = FALSE)

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
  if(is_obs&&((type=="dynamic"&&select_dyn=="sim")||
              (type=="dynamic"&&select_dyn=="common")||type=="scatter")){
    # Plot all simulations, and only obs that are simulated
    obs= obs[,intersect(colnames(obs),colnames(sim))]
  }

  if(select_dyn=="obs"||select_dyn=="common"||type=="scatter"){
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

  if(all_situations){
    melt_vars= c(melt_vars,"Sit_Name")
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

    if(select_dyn=="obs"||select_dyn=="common"||type=="scatter"){
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
      # No common variables between obs and sim (case where select_dyn=="common" or type=="scatter")
      return(list(df= obs, coloring= coloring))
    }else{
      df$variable= as.character(df$variable)
    }

    df= dplyr::full_join(df,obs,by=c(melt_vars,"variable"))

  }

  return(list(df= df, coloring= coloring))
}

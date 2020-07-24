#' Format simulation data and observation data in order to consider all situations
#'
#' @description Format simulation data and observation data in a list of a single data frame named "all_situations"
#'
#' @param list_sim A list (each element= version) of a list (each element= situation) of simulations `data.frame`s
#' @param obs A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param situations A list of names of situations
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A list of two : a list (each element= version) of a list of a single simulations `data.frame`
#' named "all_situations" and a list of a single observations `data.frame` named "all_situations"
#'
#' @keywords internal
#'
cat_situations= function(list_sim=NULL,obs=NULL,obs_sd=NULL,situations=NULL,force=TRUE,verbose=TRUE){

  if(is.null(situations)){
    if(is.null(obs) && is.null(list_sim)){
      # No simulations or observations to format
      if(verbose){
        cli::cli_alert_warning("No simulations or observations found")
      }
      if(force){
        return(NULL)
      }else{
        stop("No simulations or observations found")
      }
    }

    situations= names(list_sim[[1]])
    if(length(list_sim)>1){
      for(indice in 2:length(list_sim)){
        situations= intersect(situations,names(list_sim[[indice]]))
      }
    }
    if(!is.null(obs)){
      situations= intersect(situations, names(obs))
    }
  }

  list_sim=
    lapply(list_sim,function(x){
      for(sit_name in situations){
        # Add column with the corresponding situation name in order to properly format the data
        x[[sit_name]]=dplyr::bind_cols(x[[sit_name]],data.frame("Sit_Name"=rep(sit_name,nrow(x[[sit_name]]))))

        if(sit_name==situations[1]){
          allsim= x[[sit_name]]
          next()
        }
        allsim= dplyr::bind_rows(allsim,x[[sit_name]])
      }

      # Add dominance and plant in sim data if one of the situations is a mixture
      is_Dominance= grep("Dominance",x = colnames(allsim), fixed = TRUE)
      if(length(is_Dominance)>0){
        is_mixture= length(unique(allsim[[is_Dominance]]))>1
      }else{
        is_mixture= FALSE
      }
      if(is_mixture){
        for(sit_name in situations){
          if(length(unique(obs[[sit_name]]$Plant))==1){
            allsim$Plant[allsim$Sit_Name==sit_name]=
              rep(unique(obs[[sit_name]]$Plant),length(allsim$Plant[allsim$Sit_Name==sit_name]))
          }
        }
      }

      allsim= list(allsim)
      names(allsim)= "all_situations"
      class(allsim)= "stics_simulation"
      allsim
    })

  if(!is.null(obs)) {
    for(sit_name in situations){
      # Add column with the corresponding situation name in order to properly format the data
      obs[[sit_name]]=dplyr::bind_cols(obs[[sit_name]],data.frame("Sit_Name"=rep(sit_name,nrow(obs[[sit_name]]))))
      if(sit_name==situations[1]){
        allobs=obs[[sit_name]]
        next()
      }
      allobs= dplyr::bind_rows(allobs,obs[[sit_name]])
    }
    allobs= list(allobs)
    names(allobs)= "all_situations"
    class(allobs)= "stics_observation"
    obs= allobs
  }

  if(!is.null(obs_sd)) {
    for(sit_name in situations){
      # Add column with the corresponding situation name in order to properly format the data
      obs_sd[[sit_name]]=dplyr::bind_cols(obs_sd[[sit_name]],data.frame("Sit_Name"=rep(sit_name,nrow(obs_sd[[sit_name]]))))
      if(sit_name==situations[1]){
        allobs_sd=obs_sd[[sit_name]]
        next()
      }
      allobs_sd= dplyr::bind_rows(allobs_sd,obs_sd[[sit_name]])
    }
    allobs_sd= list(allobs_sd)
    names(allobs_sd)= "all_situations"
    class(allobs_sd)= "stics_observation"
    obs_sd= allobs_sd
  }

  return(list(list_sim,obs,obs_sd))
}


#' Format simulation data and observation data in order to represent some situations as a contiguous sequence
#'
#' @description Format simulation data and observation data in a list of data frame(s), each corresponding to one
#' situation or several contiguous situations over time
#'
#' @param list_sim A list (each element= version) of a list (each element= situation) of simulations `data.frame`s
#' @param obs A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param successive A list of lists containing the situations to be represented as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param force Continue if the plot is not possible ? E.g. no observations for scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A list of two : a list (each element= version) of a list of simulations `data.frame` and
#' a list of observations `data.frame`
#'
#' @keywords internal
#'
cat_successive=function(list_sim,obs,successive=NULL,force=TRUE,verbose=TRUE){

  if(is.null(obs) && is.null(list_sim)){
    # No simulations or observations to format
    if(verbose){
      cli::cli_alert_warning("No simulations or observations found")
    }
    if(force){
      return(NULL)
    }else{
      stop("No simulations or observations found")
    }
  }

  if(is.null(successive)){
    # No situations to put together
    if(verbose){
      cli::cli_alert_warning("No situations to join found")
    }
  }

  obs=
    lapply(successive, function(x){
      new_name=""
      col_obs=c()
      new_obs=data.frame()
      for(sit in x){
        new_name= paste0(new_name,sit," | ")
        new_obs= dplyr::bind_rows(new_obs,obs[[sit]])
        col_obs= c(col_obs,rep(sit,nrow(obs[[sit]])))
        obs[[sit]]=NULL
      }
      obs[[new_name]]= dplyr::bind_cols(new_obs,data.frame("Sit_Name"=col_obs))
      obs
    })
  obs=obs[[1]]

  list_sim=
    lapply(list_sim,function(sim){
      sim=
        lapply(successive, function(x){
          new_name=""
          col_sim=c()
          new_sim=data.frame()
          for(sit in x){
            if(!(sit%in%names(sim))){
              if(verbose){
                cli::cli_alert_warning("Situations in `successive` not found in simulation data")
              }
              if(force){
                return(NULL)
              }else{
                stop("Please enter valid situations in `succesive` parameter")
              }
            }
            new_name= paste0(new_name,sit," | ")
            new_sim= dplyr::bind_rows(new_sim,sim[[sit]])
            col_sim= c(col_sim,rep(sit,nrow(sim[[sit]])))
            sim[[sit]]=NULL
          }
          sim[[new_name]]= dplyr::bind_cols(new_sim,data.frame("Sit_Name"=col_sim))
          sim
        })
    })
  list_sim=list_sim[[1]]

  return(list(list_sim,obs))
}

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
#' @export
#'
cat_situations= function(list_sim=NULL,obs=NULL,situations=NULL,force=TRUE,verbose=TRUE){

  if(is.null(situations)){
    if(!is.null(obs)){
      situations= names(obs)
    } else if(!is.null(list_sim)){
      situations= names(list_sim[[1]])
    } else{
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
  }

  if(!is.null(list_sim)){
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
        allsim= list(allsim)
        names(allsim)= "all_situations"
        class(allsim)= "stics_simulation"
        allsim
      })
  }

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

  return(list(list_sim,obs))
}

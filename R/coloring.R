#' @description Manages the aesthetics of the graphics
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param overlap A list of lists containing the variables to represent on the same graph
#' when `type = "dynamic"`
#' @param rotation A list of lists containing the situations to be represented as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered)
#'
#' @return A coloring expression which will allow to manage the aesthetics of the graphics
#'
#' @keywords internal
#'
coloring= function(sim,obs=NULL,type=c("dynamic","scatter"),overlap=NULL,rotation=NULL,
                   one_version=TRUE,first_sim=TRUE){
  is_Dominance= grep("Dominance",x = colnames(sim), fixed = TRUE)
  if(length(is_Dominance)>0){
    is_mixture= length(unique(sim[[is_Dominance]]))>1
  }else{
    is_mixture= FALSE
  }
  is_mixture= is_mixture&&(length(unique(sim$Dominance))>1)


  coloring=list("color"=list(NULL),"shape"=list(NULL),"linetype"=list(NULL))

  # Case where there is only one item to take into account
  if(is_mixture && one_version && is.null(overlap)){
    coloring$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
  }else if(!is_mixture && !one_version && is.null(overlap)){
    if(first_sim){
      coloring$color= list("Versions"= quote(paste(names(dot_args[1]))))
    }else{
      coloring$color= list("Versions"= quote(paste(names(dot_args[i]))))
    }
  }else if(!is_mixture && one_version && !is.null(overlap)){
    coloring$color= list("Variable"= quote(.data$variable))
  }


  # Case where there are two items to take into account
  if(type=="dynamic"){
    if(is_mixture && !one_version && is.null(overlap)){
      if(first_sim){
        coloring$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
        coloring$shape= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        coloring$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
        coloring$shape= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      coloring$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && !is.null(overlap)){
      if(first_sim){
        coloring$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
        coloring$shape= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        coloring$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
        coloring$shape= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      coloring$color= list("Variable"= quote(.data$variable))
    }else if(is_mixture && one_version && !is.null(overlap)){
      coloring$linetype= list("Variable"= quote(.data$variable))
      coloring$shape= list("Variable"= quote(.data$variable))
      coloring$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }
  }

  if(type=="scatter"){
    if(is_mixture && !one_version && is.null(overlap)){
      if(first_sim){
        coloring$color= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        coloring$color= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      coloring$shape= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && !is.null(overlap)){
      if(first_sim){
        coloring$color= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        coloring$color= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      coloring$shape= list("Variable"= quote(.data$variable))
    }else if(is_mixture && one_version && !is.null(overlap)){
      coloring$color= list("Variable"= quote(.data$variable))
      coloring$shape= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }
  }

  # Case where there are three items to take into account

  return(coloring)
}

#' Manages the aesthetics of the graphics
#'
#' @description Manages the shape, color and line type of the graphics according to their content
#'
#' @param sim A simulation list of data.frames
#' @param obs An observation list of data.frames
#' @param obs_sd A list of observation standard deviation data.frames
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param overlap A list of lists containing the variables to represent on the same graph
#' when `type = "dynamic"`
#' @param successive A list of lists containing the situations to be represented as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered)
#'
#' @return An aesthetics expression which will allow to manage the aesthetics of the graphics
#'
#' @keywords internal
#'
aesthetics= function(sim,obs=NULL,type=c("dynamic","scatter"),overlap=NULL,several_sit=FALSE,
                     shape_sit=c("none","txt","symbol","group"),one_version=TRUE,first_sim=TRUE){

  is_Dominance= grep("Dominance",x = colnames(sim), fixed = TRUE)
  if(length(is_Dominance)>0){
    is_mixture= length(unique(sim[[is_Dominance]]))>1
  }else{
    is_mixture= FALSE
  }
  is_mixture= is_mixture&&(length(unique(sim$Dominance))>1)


  aesthetics=list("plot"=list("color"=list(NULL),"shape"=list(NULL),"linetype"=list(NULL)),
                  "versions"=list("shape"=list(NULL),"linetype"=list(NULL)))


  # Case where there is only one item to take into account
  if(type=="dynamic"){
    if(is_mixture && one_version && is.null(overlap)){
      aesthetics$plot$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && is.null(overlap)){
      if(first_sim){
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[i]))))
      }
    }else if(!is_mixture && one_version && !is.null(overlap)){
      aesthetics$plot$color= list("Variable"= quote(.data$variable))
    }
  }
  if(type=="scatter"){
    if(is_mixture && one_version && !several_sit){
      aesthetics$plot$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && !several_sit){
      if(first_sim){
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[i]))))
      }
    }else if(!is_mixture && one_version && several_sit){
      aesthetics$plot$color= list("Situation"= quote(paste(.data$Sit_Name)))
    }
  }


  # Case where there are two items to take into account
  if(type=="dynamic"){
    if(is_mixture && !one_version && is.null(overlap)){
      if(first_sim){
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
        aesthetics$versions$shape= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
        aesthetics$versions$shape= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      aesthetics$versions$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && !is.null(overlap)){
      if(first_sim){
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
        aesthetics$versions$shape= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
        aesthetics$versions$shape= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      aesthetics$plot$color= list("Variable"= quote(.data$variable))
    }else if(is_mixture && one_version && !is.null(overlap)){
      aesthetics$plot$linetype= list("Variable"= quote(.data$variable))
      aesthetics$plot$shape= list("Variable"= quote(.data$variable))
      aesthetics$plot$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }
  }
  if(type=="scatter"){
    if(is_mixture && !one_version && !several_sit){
      if(first_sim){
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[1]))))
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[i]))))
        aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      aesthetics$plot$shape= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
    }else if(!is_mixture && !one_version && several_sit){
      if(first_sim){
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[1]))))
      }else{
        aesthetics$versions$color= list("Versions"= quote(paste(names(dot_args[i]))))
      }
      aesthetics$plot$shape= list("Situation"= quote(.data$Sit_Name))
    }else if(is_mixture && one_version && several_sit){
      aesthetics$plot$color= list("Plant"= quote(paste(.data$Dominance,":",.data$Plant)))
      if(shape_sit!="txt"){
        aesthetics$plot$shape= list("Situation"= quote(.data$Sit_Name))
      }
    }
  }


  # Case where there are three items to take into account
  if(is_mixture && !one_version && !is.null(overlap) && type=="dynamic"){
    aesthetics$versions$color= list(quote(paste(.data$Combi)))
    aesthetics$versions$shape= list(quote(paste(.data$Combi)))
    aesthetics$versions$linetype= list(quote(paste(.data$Combi)))
  }
  if(is_mixture && !one_version && several_sit && type=="scatter"){
    aesthetics$versions$color= list(quote(paste(.data$Combi)))
    if(first_sim){
      aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[1]))))
    }else{
      aesthetics$versions$linetype= list("Versions"= quote(paste(names(dot_args[i]))))
    }
    if(shape_sit!="txt"){
      aesthetics$plot$shape= list(quote(paste(.data$Combi)))
    }
  }

  return(aesthetics)
}

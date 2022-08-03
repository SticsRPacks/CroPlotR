#' Ensure the existence of a list of variables in a `cropr_input` object
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of characters
#' @param type A character
#' @param supp_args A list of supplementary arguments depending on the plot function called
#' @return A `cropr_input` object containing all variables in `parameters`.
#' @details This function will throw an error if any of the parameters can not be calculated with the given `data_object`.
#'
#' The `type` argument is only used to make the error message more specific.
#'
#' The name of the variable passed in `data_object` is captured and used for an error message.
#' @keywords internal
ensure_hardWrapper <- function(data_object, parameters, type, supp_args=NULL){
  if(is.null(data_object))
    stop(paste0("Plot ", type, " requires data object ", substitute(data_object), "."))
  res <- ensure(data_object, parameters, supp_args)
  if(!all(res$success)){
    stop("Graph type `", type, "` requires the following parameters:\n", print_missingTree(res$missing))
  }
  return(res$object)
}

#' Ensure the existence of a list of variables in a `cropr_input` object without provocing an error
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of variables that we are interested in
#' @return A `cropr_input` object containing all variables in `parameters` if calculation was successfull.
#' `NULL` if any of the demanded parameters could not be calculated.
#' @keywords internal
ensure_softWrapper <- function(data_object, parameters){
  res <- ensure(data_object, parameters)
  success <- stats::setNames(as.list(parameters[res$success]), parameters[res$success])
  success <- lapply(success, sym)
  return(list(object = res$object, found = success))
}

#' Ensure the existence of a list of variables in a `cropr_input` object
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of characters that we are interested in ,for example:summary_station_name,rainfall_cumulated
#' @param supp_args A list of supplementary arguments depending on the plot function called
#' @return A list of three elements:
#' 1. $object, a `cropr_input` object containing all variables in `parameters` if they could be calculated from the given `data_object`
#' 2. $missing, a list keeping track of missing variables that could be provided to calculate all parameters.
#' The format of the list allows it to be printed by the `print_missingTree` function.
#' 3. $success, a logical vector indicating whether the parameters could be calculated.
#' @details The function scans the package for functions of the form "ensure_*parameter_name*".
#' Only functions found by this scan will be used to calculate new parameters.
#' @keywords internal
ensure <- function(data_object, parameters,supp_args=NULL){
  is_present <- sapply(parameters, `%in%`, unlist(sapply(utils::head(data_object, -1), names)))

  missing <- NULL
  success <- rep(TRUE, length(parameters))
  for(parameter in parameters[!is_present]){
    funName <- paste0("ensure_", parameter)
    if(exists(funName, where=asNamespace('CroPlotR'), mode='function')){
      args <- funName %>% formals() %>% utils::head()
      for(name in names(args)){
        args[[name]] <- as.name(name)
      }
      args_tmp <- supp_args[intersect(names(supp_args),names(args))]
      res <- do.call(funName,c(list(data_object),args_tmp))
      data_object <- res$object
      if(!all(res$success)){
        missing[[parameter]] <- res$missing
        success[[which(parameter == parameters)]] <- FALSE
      }
    } else{
      missing[[parameter]] <- NA
      success[[which(parameter == parameters)]] <- FALSE
    }

  }
  return(list(object = data_object, missing = missing, success = success))
}

#' Ensure the existence of a variable
#'
#' @param soil A `cropr_input` object containing soil data
#' @param weather A `cropr_input` object containing weather data
#' @return A list of three elements:
#' 1. $object, a `cropr_input` object containing the variable in the function name if it could be calculated from the given data object
#' 2. $missing, a list keeping track of missing variables that could be provided to calculate the variable.
#' The format of the list allows it to be printed by the `print_missingTree` function.
#' 3. $success, a logical value indicating whether the variable could be calculated.
#' @keywords internal
#' @name specific_ensure_doc
NULL

#' Print a list as returned by the `ensure` function's $missing return
#'
#' @param missing A list
#' @param level Level of indention
#' @return A string that prints `missing` as a AND/OR tree in a very concise way
print_missingTree <- function(missing, level = 0){
  output_children <-
    mapply(function(paramName, missing){
      output <- paste0("`", paramName, "`")
      if(is.list(missing))
        output <-
          paste0(output,
                 "\n",
                 paste(rep("\t", level+1), collapse=""),
                 "OR  ",
                 print_missingTree(missing, level+1)
          )
      return(output)
    }, names(missing), missing)
  compensate_lvl0 <- if(level==0) "  " else ""
  collapse <- paste0(
    "\n",
    compensate_lvl0,
    paste(rep("\t", level), collapse=""),
    "AND "
  )
  return(paste0(compensate_lvl0, paste(output_children, collapse = collapse)))
}

#' Get current values of function arguments
#'
#' @return A list containing the values of the parent function's arguments at that `get_argValues` is called.
#'
get_argValues <- function(){
  # get formals for parent function
  parent_formals <- formals(sys.function(sys.parent(n = 1)))
  parent_formals <- names(parent_formals)
  # transform to name
  symbols <- lapply(parent_formals, as.symbol)
  # name the list
  names(symbols) <- parent_formals
  # evaluate in parent frame
  argValues <- lapply(symbols, eval, envir = parent.frame(), enclos = emptyenv())
  # it they exist remove NULL elements and return
  argValues_isNull <- sapply(argValues, is.null)
  if(sum(argValues_isNull) > 0)
    return(argValues[-which(argValues_isNull)])
  return(argValues)
}

# get_dictFromCall <- function(function.call){
#   # get variable names
#   dict <- function.call %>%
#     # transform to list
#     as.list() %>%
#     # remove the first two elements (function name and data arguments) that are no variable names
#     utils::tail(-2)
#
#   return(dict)
# }

get_plotFunName <- function(type){
  paste0("plot__", type)
}

# get_chars_for_type <- function(type){
#   # get all characteristics in a vector
#   all_chars <- glob.chars %>% as.list() %>% unlist()
#   # get all arguemnts of the plot type function
#   req_chars <- get_plotFunName(type) %>% formals() %>% names()
#   # only keep those that are supported by the data functions
#   subset(req_chars, req_chars %in% all_chars)
# }

# get_all_possible_types <- function(soil=NULL, weather=NULL, situation=NULL){
#   # get all agrument names as list
#   chars_available <- list(soil, weather, situation) %>%
#     # we only want the names
#     lapply(names) %>%
#     # transform to vector
#     unlist()
#   # get all possible plot types and their required characterstics
#     # vector of all plot types
#   types <- glob.types %>% as.list() %>% unlist()
#     # set names to keep plot type
#   types <- stats::setNames(types, types)
#     # for each type, get required characteristics
#   types <- sapply(types, get_chars_for_type)
#   # get plot types for which all characteristics are available
#   possible <- sapply(types, function(x) all(x %in% chars_available))
#   # return plot types for which all data is present in the data objects given as arguments
#   types[possible] %>% names()
# }

data_setUnits <- function(data, unitsCol){
  data_hasUnit <- names(data) %in% names(unitsCol)
  data[data_hasUnit] <- purrr::modify2(
    data[data_hasUnit],
    unitsCol[names(data)[data_hasUnit]],
    units::set_units,
    mode="standard"
  )
  return(data)
}

get_dict <- function(vec, dict){
  new_names <- sapply(vec, function(x) names(dict)[dict == x])
  was_found <- !sapply(new_names, rlang::is_empty)
  vec[was_found] <- unlist(new_names)
  return(vec)
}

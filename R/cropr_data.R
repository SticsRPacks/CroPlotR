# create a new envirmonment to handle package realted information that should be accessible in all functions
glob.chars <- new.env(parent=emptyenv())

#' Collect soil characteristics for further use in other CorPlotR-functions
#'
#' Transforms user-provided soil data into an object that can be read by other CroPlotR
#' functions, most notably by the plot functions. Varibles can either be given explicitly
#' (eg. var1 = 5) or through a name refering to a variable in data (eg. var2 = mswc, where
#' data$mswc exists).
#'
#'
#' @param data A data.frame or list containing soil caracteristics
#' @param name Soil name
#' @param thickness Soil thickness (sum of all layers).
#' @param mswc Soil maximum soil water content (sum of all layers).
#' @param norg Soil organic nitrogen content.
#' @param var5 Some other parameter.
#' @return A list containing the given variables with fixed CroPlotR names of class \code{cropr_input}.
#' @export
#' @examples
#' \dontrun{
#' # ToDo: add data that makes this example work
#' workspace= "path_to_workspace"
#'
#' soil_data <- SticsRFiles::get_param_xml(file.path(workspace, "sols.xml"))[[1]]
#' soil_data$name <- SticsRFiles::get_soils_list(file.path(workspace, "sols.xml"))
#'
#' soil_data$MSWC <- soil_data$epc*(soil_data$HCCF-soil_data$HMINF)
#' soil_data$MSWC <- colMeans(matrix(soil_data$MSWC, nrow=5))
#' soil_data$thickness <- colSums(matrix(soil_data$epc, nrow=5))
#' soil <- data_soil(data = soil_data, thickness=thickness, mswc=MSWC, norg=norg, name=name)
#' }
#'

# set_soil <- function (x, ...) {
#   UseMethod("set_soil", x)
# }
#' #' @export
#' set_soil.list <- function(list, layer_depth=NULL, layer_water_field_cap=NULL, layer_water_wilting_pt=NULL,
#'                           layer_bulk_density_moist=NULL, organic_N_conc=NULL){
#'   data <- dplyr::bind_rows(list, .id = "id")
#'   return(set_soil.data.frame(
#'     data,
#'     id = id,
#'     layer_depth = !! subsitute(layer_depth),
#'     layer_water_field_cap = !! substitute(layer_water_field_cap),
#'     layer_water_wilting_pt = !! subsitute(layer_water_wilting_pt),
#'     layer_bulk_density_moist = !! subsitute(layer_bulk_density_moist),
#'     organic_N_conc = !! subsitute(organic_N_conc)
#'   ))
#' }



# get_indLayer <- function(data){
#   # split columns at maximum value
#   splitCols <- lapply(data, function(x) x[!is.na(x)]) %>%
#     sapply(function(x) split(x[x!=max(x)], cumsum(x==5)[x!=max(x)]))
#   # get the number of splits for each column
#   nbSplit <- sapply(splitCols, length)
#   # for each column, count the number of unsorted fragments
#   splitCols <- splitCols %>% sapply(lapply, is.unsorted) %>% sapply(unlist) %>% sapply(sum)
#   # return the column index with a high amount of splits, but few them unordered
#   indLayer <- which.min((splitCols+1)/nbSplit)
#   return(indLayer)
# }

# get_indValue <- function(data){
#   find_valueCol <- sapply(data, function(x) length(unique(x)))
#   ind_value <- which.max(find_valueCol)
# }
#
# complete_soil <- function(soil, get_errorMessage = NULL){
#
#   # saturated_wtr_cont
#   if(!"layer_saturated_wtr_cont" %in% names(soil$data_byLayer) &
#     all(
#       c("layer_depth",
#       "layer_bulk_density_moist",
#       "layer_water_field_cap",
#       "layer_water_wilting_pt") %in%
#       names(soil$data_byLayer)
#     )){
#     soil$data_byLayer$layer_saturated_wtr_cont <-
#       soil$data_byLayer %>%
#       dplyr::summarise(layer_saturated_wtr_cont =
#         layer_depth* layer_bulk_density_moist * ( layer_water_field_cap - layer_water_wilting_pt ) * 0.1
#       ) %>%
#       dplyr::pull(layer_saturated_wtr_cont)
#   }
#
#   if(!"saturated_wtr_cont" %in% names(soil$data) &
#      "layer_saturated_wtr_cont" %in% names(soil$data_byLayer)){
#     soil$data$saturated_wtr_cont <-
#       soil$data_byLayer %>%
#       dplyr::group_by(id) %>%
#       dplyr::summarise(saturated_wtr_cont = sum(layer_saturated_wtr_cont)) %>%
#       dplyr::pull(saturated_wtr_cont)
#   }
#
#   # depth
#   if(!"depth" %in% names(soil$data) &
#      "layer_depth" %in% names(soil$data_byLayer)){
#     soil$data$depth <-
#       soil$data_byLayer %>%
#       dplyr::group_by(id) %>%
#       dplyr::summarise(depth = sum(layer_depth)) %>%
#       dplyr::pull(depth)
#   }
#
#   return(soil)
# }
#'
#' #' @export
#' set_weather <- function (x, ...) {
#'   UseMethod("set_weather", x)
#' }
#'
#'
#'
#' #' @export
#' set_weather.data.frame <- function(data, id = NULL, station_name = NULL, temp_day_max= NULL, temp_day_min = NULL,
#'                                    year=NULL){
#'   # get dictionnary from function argument values
#'   dict <- tail(get_argValues(), -1)
#'   # transform character to symbol
#'   dict <- lapply(dict, as.symbol)
#'   # create and return cropr_input object containing data and dictionnary
#'   object <- list(data = data, dict = dict) %>%
#'     structure(class = "cropr_input")
#'   return(invisible(object))
#' }

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

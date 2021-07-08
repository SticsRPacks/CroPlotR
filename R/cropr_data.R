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
set_soil <- function (x, ...) {
  UseMethod("set_soil", x)
}

#' @export
set_soil.list <- function(list, name=NULL, thickness=NULL, mswc=NULL, norg=NULL, var5=NULL){
  data <- dplyr::bind_rows(list, .id = "situation")
  return(set_soil.data.frame(data, name=NULL, thickness=NULL, mswc=NULL, norg=NULL, var5=NULL))
}

#' @export
set_soil.data.frame <- function(data, name=NULL, thickness=NULL, mswc=NULL, norg=NULL, var5=NULL){

  dict <- get_dictFromCall(match.call())
  object <- list(data = data, dict = dict) %>%
    structure(class = "cropr_input")

  return(object)
  # ToDo: verify coherence of input data (same number of observations, ...)
  # ToDo: check that ... contains only named arguments
}
# copy charactersitcs automatically from function agruments
# glob.chars$soil <- data_soil %>% formals() %>% names() %>% utils::tail(-1)

#' @export
set_weather <- function (x, ...) {
  UseMethod("set_weather", x)
}

#' @export
set_weather.list <- function(list, Tmax=NULL, Tmin=NULL, Site=NULL, Year=NULL){
  data <- dplyr::bind_rows(list, .id = "situation")
  return(set_weather.data.frame(data, Tmax=NULL, Tmin=NULL, Site=NULL, Year=NULL))
}

#' @export
set_weather.data.frame <- function(data, Tmax=NULL, Tmin=NULL, Site=NULL, Year=NULL){

  dict <- get_dictFromCall(match.call())
  object <- list(data = data, dict = dict) %>%
    structure(class = "cropr_input")

  return(invisible(object))
}

get_dictFromCall <- function(function.call){
  # get variable names
  dict <- function.call %>%
    # transform to list
    as.list() %>%
    # remove the first two elements (function name and data arguments) that are no variable names
    utils::tail(-2)

  return(dict)
}

get_plotFunName <- function(type){
  paste0("plot_", type)
}

get_chars_for_type <- function(type){
  # get all characteristics in a vector
  all_chars <- glob.chars %>% as.list() %>% unlist()
  # get all arguemnts of the plot type function
  req_chars <- get_plotFunName(type) %>% formals() %>% names()
  # only keep those that are supported by the data functions
  subset(req_chars, req_chars %in% all_chars)
}

get_all_possible_types <- function(soil=NULL, weather=NULL, situation=NULL){
  # get all agrument names as list
  chars_available <- list(soil, weather, situation) %>%
    # we only want the names
    lapply(names) %>%
    # transform to vector
    unlist()
  # get all possible plot types and their required characterstics
    # vector of all plot types
  types <- glob.types %>% as.list() %>% unlist()
    # set names to keep plot type
  types <- stats::setNames(types, types)
    # for each type, get required characteristics
  types <- sapply(types, get_chars_for_type)
  # get plot types for which all characteristics are available
  possible <- sapply(types, function(x) all(x %in% chars_available))
  # return plot types for which all data is present in the data objects given as arguments
  types[possible] %>% names()
}

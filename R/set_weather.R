#' Collect data on weather variables for further use in plot-functions
#'
#' Transforms user-provided weather data into an object that can be read by other CroPlotR
#' functions, most notably by the plot functions. The data on all variables has to be given through
#' a single data frame. The names of every variable and its unit have to be specified.
#'
#' @param data A data.frame containing weather data
#' @param id Unique weather identification
#' @param station_name Weather station name
#' @param temp_day_max Air temperature, daily maximum
#' @param temp_day_min  Air temperature, daily minimum
#' @param temp_day_mean Air temperature, daily mean
#' @param rainfall_day Rainfall, per day
#' @param year year of observation
#' @param verbose Provide extra information about the function's inner procedures?
#' @return A list of class `cropr_input` containing all necessary information for plotting.
#' @details
#' The column name in `data` and the unit of every variable have to be given as a list of two characters.
#' @export
set_weather <- function(data, id = NULL, station_name=NULL, temp_day_max = NULL, temp_day_min = NULL, temp_day_mean = NULL, rainfall_day = NULL, year = NULL, verbose = FALSE){
  # in case data is a list of lists, unnest these inner lists
  data <- bind_list(data, id)

  # get dictionary from function argument values
  dict <- get_argValues()
  dict[c("data", "verbose")] <- NULL
  # seperate dictionary from units
  dict_hasUnit <- sapply(dict, is.list)
  unitsParam <- sapply(dict[dict_hasUnit], `[[`, 2)
  dict[dict_hasUnit] <- sapply(dict[dict_hasUnit], `[[`, 1)

  dict_byDay <- names(data)
  data_byDay <- data
  name_id <- if(is.null(dict[["id"]])) "id" else dict[["id"]]
  data <- data %>% dplyr::select(name_id) %>% unique()

  # update data names according to dictionary
  names(data) <- get_dict(names(data), dict)
  names(data_byDay) <- get_dict(names(data_byDay), dict)

  # apply units to respective columns
  data <- data_setUnits(data, unitsParam)
  data_byDay <- data_setUnits(data_byDay, unitsParam)

  # build weather data object
  weather <- list(data = data, data_byDay = data_byDay, dict = dict) %>%
    structure(class = "cropr_input")

  return(invisible(weather))
}

bind_list <- function(data, id){
  # unlist as long as data contains lists (only keep the name on the last level!)
  while("list" %in% sapply(data, class)){
    data <- unname(data)
    data <- unlist(data, recursive = F)
  }
  is_missingNames <- length(names(data)) != length(data)
  if(is_missingNames && is.null(id))
    stop("Every weather observation must have an identification. Please provide the `id` argument or name every weather observation in the list.")
  if(is_missingNames)
    names(data) <- sapply(data, function(x) dplyr::pull(x, !!id) %>% unique() %>% paste(collapse = ", "))
  # remove duplicats
  data <- data[unique(names(data))]
  # bind list while adding an id field
  if(is.null(id))
    data <- dplyr::bind_rows(data, .id = "id")
  else
    data <- dplyr::bind_rows(data)
  return(data)
}

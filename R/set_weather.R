#' @export
set_weather <- function(data, station_name = NULL, temp_day_max = NULL, temp_day_min = NULL, year = NULL, verbose = FALSE){
  # if data is given as list
  if("list" %in% class(data)){
    # if elements of list are themselves lists, then bind these inner lists without added id filed
    if("list" %in% sapply(data, class)){
      data <- lapply(data, dplyr::bind_rows)
    }
    # bind list while adding an id field
    data <- dplyr::bind_rows(data, .id = "id")
  }

  # get dictionnary from function argument values
  dict <- get_argValues()
  dict[c("data", "verbose")] <- NULL
  # seperate dictionary from units
  dict_hasUnit <- sapply(dict, is.list)
  unitsParam <- sapply(dict[dict_hasUnit], `[[`, 2)
  dict[dict_hasUnit] <- sapply(dict[dict_hasUnit], `[[`, 1)

  # update data names according to dictionary
  names(data) <- get_dict(names(data), dict)

  # apply units to respective columns
  data <- data_setUnits(data, unitsParam)

  # build weather data object
  weather <- list(data = data, dict = dict) %>%
    structure(class = "cropr_input")

  return(invisible(weather))
}

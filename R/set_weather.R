#' @export
set_weather <- function(data, id = NULL, station_name=NULL, temp_day_max = NULL, temp_day_min = NULL, rainfall_day = NULL, year = NULL, verbose = FALSE){
  # if data is given as list
  if("list" %in% class(data)){
    # if elements of list are themselves lists, then bind these inner lists without added id filed
    id_isNull <- is.null(id)
    if("list" %in% sapply(data, class)){
      data <- lapply(data, dplyr::bind_rows, .id = "id")
      id_isNull <- FALSE
    }
    # bind list while adding an id field
    if(id_isNull)
      data <- dplyr::bind_rows(data, .id = "id")
    else
      data <- dplyr::bind_rows(data)
  }

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

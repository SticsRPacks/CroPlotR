#' Ensure the existence of a list of variables in a `cropr_input` object
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of characters
#' @param type A character
#' @return A `cropr_input` object containing all variables in `parameters`.
#' @details This function will throw an error if any of the parameters can not be calculated with the given `data_object`.
#'
#' The `type` argument is only used to make the error message more specific.
ensure_hardWrapper <- function(data_object, parameters, type){
  res <- ensure(data_object, parameters)
  if(!all(res$success)){
    stop("Graph type `", type, "` requires the following parameters:\n", print_missingTree(res$missing))
  }
  return(res$object)
}

#' Ensure the existence of a list of variables in a `cropr_input` object without provocing an error
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of
#' @param element_name Name of the element in `data_object` that any non-existing parameter should be added to
#' @return A `cropr_input` object containing all variables in `parameters` if calculation was successfull.
#' `NULL` if any of the demanded parameters could not be calculated.
#'
ensure_softWrapper <- function(data_object, parameters){
  res <- ensure(data_object, parameters)
  success <- setNames(as.list(parameters[res$success]), parameters[res$success])
  success <- lapply(success, sym)
  return(list(res$object, success))
}

#' Ensure the existence of a list of variables in a `cropr_input` object
#'
#' @param data_object A `cropr_input` object
#' @param parameters A list of characters
#' @return A list of three elements:
#' 1. $object, a `cropr_input` object containing all variables in `parameters` if they could be calculated from the given `data_object`
#' 2. $missing, a list keeping track of missing variables that could be provided to calculate all parameters.
#' The format of the list allows it to be printed by the `print_missingTree` function.
#' 3. $success, a logical vector indicating whether the parameters could be calculated.
#' @details The function scans the package for functions of the form "ensure_*parameter_name*".
#' Only functions found by this scan will be used to calculate new parameters.
ensure <- function(data_object, parameters){
  is_present <- sapply(parameters, `%in%`, unlist(sapply(head(data_object, -1), names)))

  missing <- NULL
  success <- rep(TRUE, length(parameters))
  for(parameter in parameters[!is_present]){
    funName <- paste0("ensure_", parameter)
    if(exists(funName, where=asNamespace('CroPlotR'), mode='function')){
      res <- do.call(funName, list(data_object))
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
ensure_saturated_wtr_cap <- function(soil){
  res <- ensure(soil, "layer_saturated_wtr_cap")
  if(all(res$success))
    res$object$data$saturated_wtr_cap <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(saturated_wtr_cap = sum(layer_saturated_wtr_cap)) %>%
      dplyr::pull(saturated_wtr_cap)
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_layer_saturated_wtr_cap <- function(soil){
  res <- ensure(soil, c("layer_depth", "layer_bulk_density_moist", "layer_water_field_cap", "layer_water_wilting_pt"))
  if(all(res$success))
    res$object$data_byLayer$layer_saturated_wtr_cap <-
      res$object$data_byLayer %>%
      dplyr::summarise(layer_saturated_wtr_cap =
                         layer_depth* layer_bulk_density_moist * ( layer_water_field_cap - layer_water_wilting_pt ) * units::set_unit(0.1, "cm^2.mm/g", )
      ) %>%
      dplyr::pull(layer_saturated_wtr_cap)
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_depth <- function(soil){
  res <- ensure(soil, "layer_depth")
  if(all(res$success))
    res$object$data$depth <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(depth = sum(layer_depth)) %>%
      dplyr::pull(depth)
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_nb_below_0 <- function(weather){
  res <- ensure(weather, "temp_day_min")
  bound <- units::set_units(0, "celsius")
  bound <- units::set_units(bound, units(res$object$data_byDay$temp_day_min), mode="standard")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_below_0 = sum(temp_day_min < bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_nb_above_35 <- function(weather){
  res <- ensure(weather, "temp_day_max")
  bound <- units::set_units(35, "celsius")
  bound <- units::set_units(bound, units(res$object$data_byDay$temp_day_max), mode="standard")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_above_35 = sum(temp_day_max > bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_summary_year <- function(weather){
  res <- ensure(weather, "year")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(summary_year = paste(unique(year), collapse = ", ")) %>%
      dplyr::full_join(res$object$data, by = "id")
  }
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_summary_station_name <- function(weather){
  res <- ensure(weather, "station_name")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(summary_station_name = paste(unique(station_name), collapse = ", ")) %>%
      dplyr::full_join(res$object$data, by = "id")
  }
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_rainfall_cumulated <- function(weather){
  res <- ensure(weather, "rainfall_day")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(rainfall_cumulated = sum(rainfall_day)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_temp_day_mean <- function(weather){
  res <- ensure(weather, c("temp_day_max", "temp_day_min"))
  if(all(res$success)){
    res$object$data_byDay <-
      res$object$data_byDay %>%
      dplyr::mutate(temp_day_mean = mapply(purrr::compose(mean, c), temp_day_max, temp_day_min))
  }
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_temp_mean <- function(weather){
  res <- ensure(weather, "temp_day_mean")
  if(all(res$success)){
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(temp_mean = mean(temp_day_mean)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }
  return(res)
}


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

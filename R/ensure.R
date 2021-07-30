ensure_wrapper <- function(data_object, parameters, type){
  res <- ensure(data_object, parameters)
  if(!res$success){
    stop("Graph type `", type, "` requires the following parameters:\n", print_missingTree(res$missing))
  }
  return(res$object)
}

ensure <- function(data_object, parameters){
  is_present <- sapply(parameters, `%in%`, unlist(sapply(head(data_object, -1), names)))

  missing <- NULL
  success <- TRUE
  for(parameter in parameters[!is_present]){
    funName <- paste0("ensure_", parameter)
    if(exists(funName, where=asNamespace('CroPlotR'), mode='function')){
      res <- do.call(funName, list(data_object))
      data_object <- res$object
      if(!res$success){
        missing[[parameter]] <- res$missing
        success <- FALSE
      }
    } else{
      missing[[parameter]] <- NA
      success <- FALSE
    }

  }
  return(list(object = data_object, missing = missing, success = success))
}

ensure_saturated_wtr_cont <- function(soil){
  res <- ensure(soil, "layer_saturated_wtr_cont")
  if(res$success)
    res$object$data$saturated_wtr_cont <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(saturated_wtr_cont = sum(layer_saturated_wtr_cont)) %>%
      dplyr::pull(saturated_wtr_cont)
  return(res)
}

ensure_layer_saturated_wtr_cont <- function(soil){
  res <- ensure(soil, c("layer_depth", "layer_bulk_density_moist", "layer_water_field_cap", "layer_water_wilting_pt"))
  if(res$success)
    res$object$data_byLayer$layer_saturated_wtr_cont <-
      res$object$data_byLayer %>%
      dplyr::summarise(layer_saturated_wtr_cont =
                         layer_depth* layer_bulk_density_moist * ( layer_water_field_cap - layer_water_wilting_pt ) * 0.1
      ) %>%
      dplyr::pull(layer_saturated_wtr_cont)
  return(res)
}

ensure_depth <- function(soil){
  res <- ensure(soil, "layer_depth")
  if(res$success)
    res$object$data$depth <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(depth = sum(layer_depth)) %>%
      dplyr::pull(depth)
  return(res)
}

ensure_nb_below_0 <- function(weather){
  res <- ensure(weather, "temp_day_min")
  bound <- units::set_units(0, "celsius")
  bound <- units::set_units(bound, units(res$object$data$temp_day_min), mode="standard")
  if(res$success){
    res$object$data <-
      res$object$data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(nb_below_0 = sum(temp_day_min < bound))
  }

  return(res)
}

ensure_nb_above_35 <- function(weather){
  res <- ensure(weather, "temp_day_max")
  bound <- units::set_units(35, "celsius")
  bound <- units::set_units(bound, units(res$object$data$temp_day_max), mode="standard")
  if(res$success){
    res$object$data <-
      res$object$data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(nb_above_35 = sum(temp_day_max > bound))
  }

  return(res)
}

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

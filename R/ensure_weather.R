#' @rdname specific_ensure_doc
ensure_summary_year <- function(weather){
  summary_year <- year <- NULL
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

#' @rdname specific_ensure_doc
ensure_summary_station_name <- function(weather){
  id <- summary_station_name <- station_name <- NULL
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

#' @rdname specific_ensure_doc
ensure_rainfall_cumulated <- function(weather){
  id <- rainfall_cumulated <- rainfall_day <- NULL
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

#' @rdname specific_ensure_doc
ensure_radiation_cumulated <- function(weather){
  radiation_cumulated <- radiation_day <- NULL
  res <- ensure(weather, "radiation_day")
  if(all(res$success)){
    res$object$data_byDay <-
      res$object$data_byDay %>%
      dplyr::mutate(radiation_cumulated = cumsum(radiation_day))
  }
  return(res)
}

#' @rdname specific_ensure_doc
ensure_temp_day_mean <- function(weather){
  temp_day_mean <- temp_day_max <- temp_day_min <- NULL
  res <- ensure(weather, c("temp_day_max", "temp_day_min"))
  if(all(res$success)){
    res$object$data_byDay <-
      res$object$data_byDay %>%
      dplyr::mutate(temp_day_mean = mapply(purrr::compose(mean, c), temp_day_max, temp_day_min))
  }
  return(res)
}

#' @rdname specific_ensure_doc
ensure_temp_mean <- function(weather){
  temp_mean <- temp_day_mean <- NULL
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

#' @rdname specific_ensure_doc
ensure_nb_below_threshold_Tmin <- function(weather,threshold_Tmin){
  temp_day_min <- NULL
  res <- ensure(weather, "temp_day_min")
  if(all(res$success)){
    bound <- units::set_units(threshold_Tmin, "celsius")
    bound <- units::set_units(bound, units(res$object$data_byDay$temp_day_min), mode="standard")
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_below_threshold_Tmin = sum(temp_day_min < bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

#' @rdname specific_ensure_doc
ensure_nb_above_threshold_Tmax <- function(weather,threshold_Tmax){
  temp_day_max <- NULL
  res <- ensure(weather, "temp_day_max")
  if(all(res$success)){
    bound <- units::set_units(threshold_Tmax, "celsius")
    bound <- units::set_units(bound, units(res$object$data_byDay$temp_day_max), mode="standard")
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_above_threshold_Tmax = sum(temp_day_max > bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

ensure_nb_below_threshold_RainMin <- function(weather,threshold_RainMin){
  rainfall_day <- NULL
  res <- ensure(weather, "rainfall_day")

  if(all(res$success)){
    bound <- units::set_units(threshold_RainMin, "mm")
    bound <- units::set_units(bound, units(res$object$data_byDay$rainfall_day), mode="standard")
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_below_threshold_RainMin = sum(rainfall_day < bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

#' @rdname specific_ensure_doc
ensure_nb_above_threshold_RainMax<- function(weather,threshold_RainMax){
  rainfall_day <- NULL
  res <- ensure(weather, "rainfall_day")

  if(all(res$success)){
    bound <- units::set_units(threshold_RainMax, "mm")
    bound <- units::set_units(bound, units(res$object$data_byDay$rainfall_day), mode="standard")
    res$object$data <-
      res$object$data_byDay %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(nb_above_threshold_RainMax = sum(rainfall_day > bound)) %>%
      dplyr::full_join(res$object$data, by = "id")
  }

  return(res)
}

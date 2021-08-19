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
  saturated_wtr_cap <- layer_saturated_wtr_cap <- NULL
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
  layer_saturated_wtr_cap <- layer_depth <- layer_bulk_density_moist <- layer_water_field_cap <- layer_water_wilting_pt <- NULL
  res <- ensure(soil, c("layer_depth", "layer_bulk_density_moist", "layer_water_field_cap", "layer_water_wilting_pt"))
  if(all(res$success))
    res$object$data_byLayer$layer_saturated_wtr_cap <-
      res$object$data_byLayer %>%
      dplyr::summarise(layer_saturated_wtr_cap =
                         layer_depth* layer_bulk_density_moist * ( layer_water_field_cap - layer_water_wilting_pt ) * units::set_units(0.1, "mm cm2 g-1", mode = "standard")
      ) %>%
      dplyr::pull(layer_saturated_wtr_cap)
  return(res)
}

#' @rdname ensure_saturated_wtr_cap
ensure_depth <- function(soil){
  id <- depth <- layer_depth <- NULL
  res <- ensure(soil, "layer_depth")
  if(all(res$success))
    res$object$data$depth <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(depth = sum(layer_depth)) %>%
      dplyr::pull(depth)
  return(res)
}

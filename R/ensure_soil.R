#' @rdname specific_ensure_doc
ensure_soil_max_wtr_cap <- function(soil){
  soil_max_wtr_cap <- layer_max_wtr_cap <- NULL
  res <- ensure(soil, "layer_max_wtr_cap")
  if(all(res$success))
    res$object$data$soil_max_wtr_cap <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(soil_max_wtr_cap = sum(layer_max_wtr_cap)) %>%
      dplyr::pull(soil_max_wtr_cap)
  return(res)
}

#' @rdname specific_ensure_doc
ensure_layer_max_wtr_cap <- function(soil){
  layer_max_wtr_cap <- layer_thickness <- layer_bulk_density <- layer_water_field_cap <- layer_water_wilting_pt <- NULL
  res <- ensure(soil, c("layer_thickness", "layer_bulk_density", "layer_water_field_cap", "layer_water_wilting_pt"))
  if(all(res$success))
    res$object$data_byLayer$layer_max_wtr_cap <-
      res$object$data_byLayer %>%
      dplyr::summarise(layer_max_wtr_cap =
                         layer_thickness* layer_bulk_density * ( layer_water_field_cap - layer_water_wilting_pt ) * units::set_units(0.1, "mm cm2 g-1", mode = "standard")
      ) %>%
      dplyr::pull(layer_max_wtr_cap)
  return(res)
}

#' @rdname specific_ensure_doc
ensure_thickness <- function(soil){
  id <- thickness <- layer_thickness <- NULL
  res <- ensure(soil, "layer_thickness")
  if(all(res$success))
    res$object$data$thickness <-
      res$object$data_byLayer %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(thickness = sum(layer_thickness)) %>%
      dplyr::pull(thickness)
  return(res)
}

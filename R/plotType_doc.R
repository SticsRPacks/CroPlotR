#' Plot type thickness.mswc
#'
#' @param depth Depth of the entire soil
#' @param saturated_wtr_cap Saturated water capacity of the entire soil
#' @param organic_N_conc Organic nitrogen concentration in the soil
#' @name thickness.mswc
#' @example
#' # load example data
#' workspace <- system.file(file.path("extdata", "stics_example_input"), package = "CroPlotR")
#' soil_data <- readRDS(file.path(workspace, "soil_data_wide.rds"))
#' soil <- set_soil(
#' soil_data,
#' id = "name",
#' layer_depth = list("epc", "cm"),
#' layer_water_field_cap = list("HCCF", "g/g"),
#' layer_water_wilting_pt = list("HMINF", "g/g"),
#' layer_bulk_density_moist = list("DAF", "g/cm^3"),
#' organic_N_conc = list("norg", "g/g")
#' )
#'
#' # create plot
#' soil_plot(soil, type = "thickness.mswc")
NULL
#> NULL

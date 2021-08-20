## ---- include = FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = F, message = F-------------------------------------------------------------------------------------------------
library(CroPlotR)
library(magrittr)
workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
soil_data <- readRDS(file.path(workspace, "soil_data_long.rds"))
weather <- readRDS(file.path(workspace, "weather_france.rds"))

soil <- set_soil(
  soil_data, 
  id = "name", 
  layer_thickness = list("epc", "cm"), 
  layer_water_field_cap = list("HCCF", "g/g"),
  layer_water_wilting_pt = list("HMINF", "g/g"), 
  layer_bulk_density = list("DAF", "g/cm^3"),
  organic_N_conc = list("norg", "g/g")
)

## ---- echo = F, warning = F, fig.align = 'center', fig.width = 7, fig.height = 5--------------------------------------------
plot_soil(soil, type = "thickness.mswc")

## ---- echo = F, fig.align = 'center', fig.width = 7, fig.height = 5---------------------------------------------------------
plot_weather(weather, type = "limiting.temperatures", size=2)


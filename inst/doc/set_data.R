## ---- include = FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, message=FALSE---------------------------------------------------------------------------------------------
library(dplyr)
example_names <- c("solcanne", "solescourg")

## ---- message=FALSE---------------------------------------------------------------------------------------------------------
library(CroPlotR)
workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
soil_data_wide <- readRDS(file.path(workspace, "soil_data_wide.rds"))
soil_data_long <- readRDS(file.path(workspace, "soil_data_long.rds"))
soil_data_tibble <- readRDS(file.path(workspace, "soil_data_tibble.rds"))

## ---- echo=F----------------------------------------------------------------------------------------------------------------
intro_soil_data_tibble <- 
  soil_data_tibble %>% 
  select(c(name, norg, epc)) %>% 
  filter(name %in% example_names) 
knitr::kable(intro_soil_data_tibble) %>% kableExtra::kable_paper(full_width = F)

## ---- echo=F----------------------------------------------------------------------------------------------------------------
intro_soil_data_wide <- 
  as.data.frame(soil_data_wide) %>% 
  select(c(name, norg) | starts_with("epc")) %>% 
  filter(name %in% example_names)
knitr::kable(intro_soil_data_wide) %>% kableExtra::kable_paper(full_width = F)

## ---- echo=F----------------------------------------------------------------------------------------------------------------
intro_soil_data_long <- 
  as.data.frame(soil_data_long) %>%
  dplyr::filter(name %in% example_names & (variable %in% c("norg") | startsWith(variable, "epc")))
knitr::kable(intro_soil_data_long) %>% kableExtra::kable_paper(full_width = F)

## ---------------------------------------------------------------------------------------------------------------------------
soil <- set_soil(
  soil_data_wide, 
  id = "name",
  organic_N_conc = list("norg", "g/g"), 
  layer_thickness = list("epc", "cm")
)

## ---------------------------------------------------------------------------------------------------------------------------
soil <- set_soil(
  soil_data_wide, 
  id = "name",
  organic_N_conc = list("norg", "g/g"), 
  layer_thickness = list("epc", "cm"),
  data_format = "wide"
)

## ---------------------------------------------------------------------------------------------------------------------------
soil <- set_soil(
  soil_data_long, 
  id = "name",
  organic_N_conc = list("norg", "g/g"), 
  layer_thickness = list("epc", "cm"),
  data_format = list("long", list(id = "name", variable = "variable", layer = 3, value = "value"))
)

## ---- message=FALSE---------------------------------------------------------------------------------------------------------
library(CroPlotR)
workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
weather_data_list <- readRDS(file.path(workspace, "weather_data_list.rds"))

## ---- echo=FALSE------------------------------------------------------------------------------------------------------------
knitr::kable(head(weather_data_list[[1]][[1]], 7)) %>% kableExtra::kable_paper(full_width = F)

## ---------------------------------------------------------------------------------------------------------------------------
weather <- set_weather(
  weather_data_list, 
  id = "station", 
  temp_day_max = list("ttmax", "celsius"), 
  temp_day_min = list("ttmin", "celsius"), 
  year = "year"
)

## ---------------------------------------------------------------------------------------------------------------------------
soil <- set_soil(
  soil_data_wide, 
  id = "name",
  organic_N_conc = list("norg", "g/g"), 
  layer_thickness = list("epc", "cm")
)


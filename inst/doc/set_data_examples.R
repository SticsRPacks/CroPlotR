## ---- include = FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = F-----------------------------------------------------------------------------------------------------------
library(CroPlotR)

## ---- echo = F--------------------------------------------------------------------------------------------------------------
head.list <- function(obj, n = 2L, ...)
{
    stopifnot(length(n) == 1L)
    origN <- n
    n <- if (n < 0L)
        max(length(obj) + n, 0L)
    else min(n, length(obj))
    lapply(obj[seq_len(n)], function(x)
           {
               tryCatch({
                   head(x, origN, ...)
               }, error = function(e) {
                   x
               })
           })
}
environment(head.list) <- asNamespace('utils')

## ---------------------------------------------------------------------------------------------------------------------------
# define path to STICS data files
workspace <- "C:\\Users\\plutz\\Documents\\datasets\\JavaSTICS-1.40-stics-8.50\\example"

# charge soil data from the file `sols.xml` using the SticsRFiles package in long format, 
# add the argument `wide_shape = TRUE` for wide format
soil_data <- SticsRFiles:::get_xml_files_param_df(file_path = file.path(workspace, "sols.xml"))

# create soil object
soil <- set_soil(
  soil_data, 
  id = "name", 
  layer_thickness = list("epc", "cm"), 
  layer_water_field_cap = list("HCCF", "g/g"),
  layer_water_wilting_pt = list("HMINF", "g/g"), 
  layer_bulk_density = list("DAF", "g/cm^3"),
  organic_N_conc = list("norg", "g/g")
)

## ---------------------------------------------------------------------------------------------------------------------------
head(soil_data)

## ---------------------------------------------------------------------------------------------------------------------------
# define path to STICS data files
workspace <- "C:\\Users\\plutz\\Documents\\datasets\\JavaSTICS-1.40-stics-8.50\\example"

# get the list of all simulation unit names
usm_list <- SticsRFiles:::get_usms_list(file.path(workspace,"usms.xml"))

# get the list of all weather file names and name it
meteo_usms <- lapply(usm_list, function(x) 
  unique(unlist(SticsRFiles:::get_param_xml(file.path(workspace,"usms.xml"), 
                                            param_name = c("fclim1", "fclim2"), 
                                            select = "usm", 
                                            value = x)))
  )
names(meteo_usms) <- usm_list

# list of weather data for each simulation unit, 
# every element is a sub-list containing the weather data in one or multiple data.frame(s)
weather_data <- lapply(meteo_usms , function(x) {
  m <- lapply(x, function(y) SticsRFiles:::get_climate_txt(dirpath=workspace, filename = y))
  names(m) <- x 
  m
})

weather <- set_weather(
  weather_data,
  station_name = "station",
  temp_day_max = list("ttmax", "celsius"),
  temp_day_min = list("ttmin", "celsius"),
  year = "year"
)

## ---------------------------------------------------------------------------------------------------------------------------
head(usm_list)
head(meteo_usms)
# head(weather_data)  # -> produces an error during package build ?

## ---- warning = F, message = F----------------------------------------------------------------------------------------------
library(DSSAT)
soil_file <- "C:\\Users\\plutz\\Documents\\DSSAT\\AllSoils\\AU.SOL"

soil_data <- read_sol(soil_file)
soil <- set_soil(soil_data, id="SITE", soil_max_wtr_cap="SSAT")

## ---------------------------------------------------------------------------------------------------------------------------
head(soil_data)

## ---- warning = F-----------------------------------------------------------------------------------------------------------
library(DSSAT)
workpath <- "C:\\Users\\plutz\\Documents\\DSSAT\\AllWeather"

weather_names <- list.files(workpath, "\\.WTH$")
names(weather_names) <- weather_names

weather_data <- lapply(weather_names, function(x) read_wth(file.path(workpath, x)))
weather_data <- lapply(weather_data, function(x){ x$year <- format(x$DATE, format = "%Y"); x})

weather <- set_weather(
  weather_data,
  temp_day_max = list("TMAX", "celsius"),
  temp_day_min = list("TMIN", "celsius"),
  year = "year"
)

## ---------------------------------------------------------------------------------------------------------------------------
head(weather_names)
head(weather_data)


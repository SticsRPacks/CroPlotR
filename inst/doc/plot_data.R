## ---- include = FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE---------------------------------------------------------------------------------------------------------
library(CroPlotR)

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
soil_data <- readRDS(file.path(workspace, "soil_data_long.rds"))
weather_data <- readRDS(file.path(workspace, "weather_data_list.rds"))

soil <- set_soil(
  soil_data, 
  id = "name", 
  layer_thickness = list("epc", "cm"), 
  layer_water_field_cap = list("HCCF", "g/g"),
  layer_water_wilting_pt = list("HMINF", "g/g"), 
  layer_bulk_density = list("DAF", "g/cm^3"),
  organic_N_conc = list("norg", "g/g")
)

weather <- set_weather(
  weather_data,
  station_name = "station",
  temp_day_max = list("ttmax", "celsius"),
  temp_day_min = list("ttmin", "celsius"),
  year = "year"
)

## ----fig.align="center", fig.width = 7, fig.height = 5, warning=FALSE-------------------------------------------------------
plot_soil(soil, type="thickness.mswc")

## ---- warning = FALSE, fig.width = 7, fig.height = 5------------------------------------------------------------------------
list_plots <- plot_soil(soil, type="all", verbose = TRUE)
list_plots[["thickness.mswc"]]

## ----fig.align="center", fig.width = 7, fig.height = 5, warning=FALSE-------------------------------------------------------
plot_soil(soil, type="thickness.mswc", interactive = TRUE)

## ----fig.align="center", fig.width = 7, fig.height = 5, warning=FALSE, message = FALSE--------------------------------------
library(ggrepel)
p <- plot_soil(soil, type="thickness.mswc")
geomTextRepel(p) <- geom_text_repel(max.overlaps = Inf)
p

## ----histogram, fig.width = 7, fig.height = 5-------------------------------------------------------------------------------
plot_soil(soil, type="thickness.mswc", histogram = TRUE)

## ---- fig.align="center", fig.width = 7, fig.height = 5, warning=FALSE------------------------------------------------------
library(ggplot2)
plot_soil(soil, type="thickness.mswc") + 
  # overwrite label on x axis
  xlab("My modified label on x axis") + 
  # add horizontal red line
  geom_abline(slope = 3.6, intercept = -90, color = "red")

## ---- warning=FALSE, fig.align="center", fig.width = 7, fig.height = 5------------------------------------------------------
library(ggplot2)
plot_soil(soil, type = "thickness.mswc", size = 2, alpha = 0.3)

## ---- warning=FALSE, fig.align="center", fig.width = 7, fig.height = 5------------------------------------------------------
library(ggplot2)
plot_soil(soil, 
          type = "thickness.mswc", 
          # removing color scale that is usually present on this graph type
          colour = 1, 
          # adding our own size scale to the graph
          mapping = aes(size = organic_N_conc)) +
  # adding a label for our new size scale
  labs(size = "Organic N conc.")

## ---- warning=FALSE, fig.align="center", fig.width = 7, fig.height = 5, message = F-----------------------------------------
library(ggplot2)
library(units)

plot_soil(
  soil, 
  type = "thickness.mswc", 
  mapping = aes(
    colour = ifelse(
      organic_N_conc >= set_units(0.2, "g/g"), 
      "\u2265 0.2 g/g", 
      "< 0.2 g/g"
    )
  )
)


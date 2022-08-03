context("testing plot_weather function")
library(ggplot2)
library(units)
library(testthat)
workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
weather_data_list<- readRDS(file.path(workspace, "weather_data_list.rds"))

weather <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"),
                       temp_day_min = list("ttmin", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"))

test_that("limiting.temperatures basic plot",{
  plot <- plot_weather(weather, type="limiting.temperatures")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-limiting.temperatures-basic", plot)
  )
})

test_that("limiting.rainfall_days basic plot",{
  plot <- plot_weather(weather, type="limiting.rainfall_days")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-limiting.rainfall_days-basic", plot)
  )
})

test_that("temperature.rainfall basic plot",{
  plot <- plot_weather(weather, type="temperature.rainfall")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-temperature.rainfall-basic", plot)
  )
})

test_that("temperature basic plot",{
  plot <- plot_weather(weather, type="temperature")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-temperature-basic", plot)
  )
})

test_that("radiation basic plot",{
  plot <- plot_weather(weather, type="radiation")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-radiation-basic", plot)
  )
})

test_that("radiation cumulated basic plot",{
  plot <- plot_weather(weather, type="radiation",cumulate = TRUE)
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-radiation_cumulated-basic", plot)
  )
})


test_that("limiting.temperatures histogram plot",{
  plot <- plot_weather(weather, type="limiting.temperatures", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-limiting.temperatures-hist", plot)
})


weather_without_temp_day_max <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"))
weather_without_temp_day_min <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"),year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"))
weather_without_rainfall <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), temp_day_max = list("ttmax", "celsius"), year = "year",radiation_day = list("ttrg", "MJ.m-2"))
weather_without_temp <- set_weather(weather_data_list, station_name = "station", year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"))
weather_without_radiation <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), temp_day_max = list("ttmax", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"))

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_temp_day_max, type="all", verbose = TRUE), regexp = "Could not plot limiting.temperatures")
  expect_error(plot_weather(weather_without_temp_day_max, type="limiting.temperatures", verbose = TRUE), regexp = "Graph type `limiting.temperatures` requires the following parameters")
})


test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_temp_day_min, type="all", verbose = TRUE), regexp = "Could not plot limiting.temperatures")
  expect_error(plot_weather(weather_without_temp_day_min, type="limiting.temperatures", verbose = TRUE), regexp = "Graph type `limiting.temperatures` requires the following parameters")
})


test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_rainfall, type="all", verbose = TRUE), regexp = "Could not plot temperature.rainfall")
  expect_error(plot_weather(weather_without_rainfall, type="temperature.rainfall", verbose = TRUE), regexp = "Graph type `temperature.rainfall` requires the following parameters")
})


test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_temp, type="all", verbose = TRUE), regexp = "Could not plot temperature.rainfall")
  expect_error(plot_weather(weather_without_temp, type="temperature.rainfall", verbose = TRUE), regexp = "Graph type `temperature.rainfall` requires the following parameters")
})

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_rainfall, type="all", verbose = TRUE), regexp = "Could not plot limiting.rainfall_days")
  expect_error(plot_weather(weather_without_rainfall, type="limiting.rainfall_days", verbose = TRUE), regexp = "Graph type `limiting.rainfall_days` requires the following parameters")
})

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_temp_day_max, type="all", verbose = TRUE), regexp = "Could not plot temperature")
  expect_error(plot_weather(weather_without_temp_day_max, type="temperature", verbose = TRUE), regexp = "Graph type `temperature` requires the following parameters")
})



test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_radiation, type="all", verbose = TRUE), regexp = "Could not plot radiation")
  expect_error(plot_weather(weather_without_radiation, type="radiation", verbose = TRUE), regexp = "Graph type `radiation` requires the following parameters")
})

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_radiation, type="all", verbose = TRUE), regexp = "Could not plot radiation cumulate")
  #expect_error(plot_weather(weather_without_radiation, type="radiation", verbose = TRUE,cumulate = TRUE), regexp = "Graph type `radiation cumulate` requires the following parameters")
})


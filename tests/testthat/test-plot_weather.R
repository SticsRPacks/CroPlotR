context("testing plot_weather function")
library(ggplot2)
library(units)
library(testthat)
workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
weather_data_list<- readRDS(file.path(workspace, "weather_data_list.rds"))

weather <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"),
                       temp_day_min = list("ttmin", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"), etp_day = list("ttetp", "mm"))

 test_that("limiting.temperatures basic plot",{
   plot <- plot_weather(weather, type="limiting.temperatures")
     vdiffr::expect_doppelganger("plot-limiting.temperatures-basic", plot)
 })

test_that("limiting.rainfall_days basic plot",{
  plot <- plot_weather(weather, type="limiting.rainfall_days")

    vdiffr::expect_doppelganger("plot-limiting.rainfall_days-basic", plot)
})

test_that("temperature.rainfall basic plot",{
  plot <- plot_weather(weather, type="temperature.rainfall")

    vdiffr::expect_doppelganger("plot-temperature.rainfall-basic", plot)

})

test_that("temperature basic plot",{
  plot <- plot_weather(weather, type="temperature")

    vdiffr::expect_doppelganger("plot-temperature-basic", plot)

})

test_that("radiation basic plot",{
  plot <- plot_weather(weather, type="radiation")

    vdiffr::expect_doppelganger("plot-radiation-basic", plot)

})

test_that("radiation cumulated basic plot",{
  plot <- plot_weather(weather, type="radiation",cumulate = TRUE)

    vdiffr::expect_doppelganger("plot-radiation_cumulated-basic", plot)

})

test_that("cumulated_PET.rain basic plot",{
  plot <- plot_weather(weather, type="cumulated_PET.rain")

    vdiffr::expect_doppelganger("plot-cumulated_PET.rain-basic", plot)

})

test_that("limiting.temperatures histogram plot",{
  plot <- plot_weather(weather, type="limiting.temperatures", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-limiting.temperatures-hist", plot)
})

test_that("limiting.rainfall_days histogram plot",{
  plot <- plot_weather(weather, type="limiting.rainfall_days", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-limiting.rainfall_days-hist", plot)
})

test_that("cumulated_PET.rain histogram plot",{
  plot <- plot_weather(weather, type="cumulated_PET.rain", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-cumulated_PET.rain-hist", plot)
})

test_that("temperature.rainfall histogram plot",{
  plot <- plot_weather(weather, type="temperature.rainfall", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-temperature.rainfall-hist", plot)
})

weather_without_temp_day_max <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"), etp_day = list("ttetp", "mm"))
weather_without_temp_day_min <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"),year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"), etp_day = list("ttetp", "mm"))
weather_without_rainfall <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), temp_day_max = list("ttmax", "celsius"), year = "year",radiation_day = list("ttrg", "MJ.m-2"), etp_day = list("ttetp", "mm"))
weather_without_temp <- set_weather(weather_data_list, station_name = "station", year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"), etp_day = list("ttetp", "mm"))
weather_without_radiation <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), temp_day_max = list("ttmax", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"), etp_day = list("ttetp", "mm"))
weather_without_etp_day <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"),temp_day_max = list("ttmax", "celsius"), year = "year",rainfall_day = list("ttrr", "mm"),radiation_day = list("ttrg", "MJ.m-2"))

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
  expect_message(plot_weather(weather_without_radiation, type="all", verbose = TRUE,cumulate = TRUE), regexp = "Could not plot radiation")
  expect_error(plot_weather(weather_without_radiation, type="radiation", verbose = TRUE,cumulate = TRUE), regexp = "Graph type `radiation` requires the following parameters")
})

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_etp_day, type="all", verbose = TRUE), regexp = "Could not plot cumulated_PET.rain")
  expect_error(plot_weather(weather_without_etp_day, type="cumulated_PET.rain", verbose = TRUE), regexp = "Graph type `cumulated_PET.rain` requires the following parameters")
})

test_that("plot weather legend : symbol mal défini",{
  expect_error(plot_weather(weather, type = "limiting.temperatures",threshold_Tmin=5,threshold_Tmax=33,symbol=5), regexp = "unexpected value for argument symbol")

})

test_that("plot weather legend : symbol=auto",{
  plot <- plot_weather(weather, type="limiting.temperatures",symbol="auto")

  vdiffr::expect_doppelganger("plot-limiting.temperatures-basic-legend : symbol=auto", plot)

})


test_that("plot weather legend : symbol=Site", {
  plot <- plot_weather(weather, type="limiting.temperatures",symbol="Site")

  vdiffr::expect_doppelganger("plot-limiting.temperatures-basic-legend : symbol=Site", plot)

})

test_that("plot weather legend : symbol=Year", {
  plot <- plot_weather(weather, type="limiting.temperatures",symbol="Year")
  vdiffr::expect_doppelganger("plot-limiting.temperatures-basic-legend : symbol=Year", plot)
})

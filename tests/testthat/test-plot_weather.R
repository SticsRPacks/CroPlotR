context("testing plot_weather function")

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
weather_data_list<- readRDS(file.path(workspace, "weather_data_list.rds"))
weather <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"), temp_day_min = list("ttmin", "celsius"), year = "year")

test_that("limiting.temperatures basic plot",{
  plot <- plot_weather(weather, type="limiting.temperatures")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-limiting.temperatures-basic", plot)
  )
})

test_that("limiting.temperatures histogram plot",{
  plot <- plot_weather(weather, type="limiting.temperatures", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-limiting.temperatures-hist", plot)
})


weather_without_temp_day_max <- set_weather(weather_data_list, station_name = "station", temp_day_min = list("ttmin", "celsius"), year = "year")

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_weather(weather_without_temp_day_max, type="all", verbose = TRUE), regexp = "Could not plot limiting.temperatures")
  expect_error(plot_weather(weather_without_temp_day_max, type="limiting.temperatures", verbose = TRUE), regexp = "Graph type `limiting.temperatures` requires the following parameters")
})

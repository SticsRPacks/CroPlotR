context("testing set_data functions")

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
weather_data_list <- readRDS(file.path(workspace, "weather_data_list.rds"))

local_edition(3)
test_that("detect weather data frame", {
  testthat::expect_snapshot(
    set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"), temp_day_min = list("ttmin", "celsius"), year = "year",
             verbose=T)
  )
})
test_that("load weather data", {
  weather <- set_weather(weather_data_list, station_name = "station", temp_day_max = list("ttmax", "celsius"), temp_day_min = list("ttmin", "celsius"), year = "year")
  testthat::expect_snapshot(str(weather))
})

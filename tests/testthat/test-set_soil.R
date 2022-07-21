library(testthat)
context("testing set_data functions")

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")

soil_data_wide <- readRDS(file.path(workspace, "soil_data_wide.rds"))
soil_data_long <- readRDS(file.path(workspace, "soil_data_long.rds"))
soil_data_tibble <- readRDS(file.path(workspace, "soil_data_tibble.rds"))

local_edition(3)
test_that("detect soil data frame wide", {
  testthat::expect_snapshot(
    set_soil(soil_data_wide, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T)
    )
})

litest_that("detect soil data frame long", {
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

test_that("detect soil data frame tibble", {
  testthat::expect_snapshot(
    set_soil(soil_data_tibble, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

test_that("soil data frame wide", {
  soil <- set_soil(soil_data_wide, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(str(soil))
})

test_that("soil data frame long", {
  soil <- set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(str(soil))
})

test_that("soil data frame tibble", {
  soil <- set_soil(soil_data_tibble, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(str(soil))
})

test_that("soil data frame wide misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_wide, id="name", layer_thickness="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("soil data frame long misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, id="name", layer_thickness="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("soil data frame tibble misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_tibble, id="name", layer_thickness="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("No data format found", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_tibble, id="name", layer_thickness="A", layer_water_field_cap="B",
             layer_water_wilting_pt="C", layer_bulk_density="D", organic_N_conc="E",
             verbose=T)
  )
})

test_that("Soil data frame long modify data columns", {
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(id="name", variable=2, layer="layer", value=4)))
  )
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(variable=2, layer="layer", value=4)))
  )
})

test_that("Soil data frame long data columns error", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(id="name", variable=3.2, layer="id", value=5)))
  )
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(variable=3, layer="id")))
  )
  soil_data_long_mod <- soil_data_long %>% dplyr::select(-c(layer, value))
  testthat::expect_snapshot_error(
    set_soil(soil_data_long_mod, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T)
  )
  soil_data_long_mod <- soil_data_long
  soil_data_long_mod$idd <- soil_data_long_mod$id
  soil_data_long_mod$valuee <- soil_data_long_mod$value
  testthat::expect_snapshot(
    set_soil(soil_data_long_mod, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

# Test that the package return an error message if units provided are not compatible with default ones.
# library(units)
# tmp=list("epc", "J")
# test_that("Soil data units", {
#   soil <- set_soil(soil_data_wide, id="name", layer_thickness=tmp, layer_water_field_cap=list("HCCF", "g/g"),
#                    layer_water_wilting_pt=list("HMINF", "g/g"), layer_bulk_density=list("DAF", "g/cm3"),
#                    organic_N_conc=list("norg", "g/g"), verbose=T)
# })



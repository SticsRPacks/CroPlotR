context("testing set_data functions")

workspace <- system.file(file.path("extdata", "stics_example_input"), package = "CroPlotR")

soil_data_large <- readRDS(file.path(workspace, "soil_data_large.rds"))
soil_data_long <- readRDS(file.path(workspace, "soil_data_long.rds"))
soil_data_list <- readRDS(file.path(workspace, "soil_data_list.rds"))

local_edition(3)
test_that("detect soil data frame large", {
  testthat::expect_snapshot(
    set_soil(soil_data_large, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T)
    )
})

test_that("detect soil data frame long", {
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

test_that("detect soil data frame list", {
  testthat::expect_snapshot(
    set_soil(soil_data_list, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

test_that("soil data frame large", {
  soil <- set_soil(soil_data_large, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(soil)
})

test_that("soil data frame long", {
  soil <- set_soil(soil_data_long, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(soil)
})

test_that("soil data frame list", {
  soil <- set_soil(soil_data_list, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
                   layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
                   verbose=T)
  testthat::expect_snapshot(soil)
})

test_that("soil data frame large misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_large, id="name", layer_depth="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("soil data frame long misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, id="name", layer_depth="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("soil data frame list misspell", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_list, id="name", layer_depth="epcc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="noGrg",
             verbose=T)
  )
})

test_that("No data format found", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_list, id="name", layer_depth="A", layer_water_field_cap="B",
             layer_water_wilting_pt="C", layer_bulk_density_moist="D", organic_N_conc="E",
             verbose=T)
  )
})

test_that("Soil data frame long modify data columns", {
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(id="name", param=3, layer="id", value=5)))
  )
  testthat::expect_snapshot(
    set_soil(soil_data_long, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(param=3, layer="id", value=5)))
  )
})

test_that("Soil data frame long data columns error", {
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(id="name", param=3.2, layer="id", value=5)))
  )
  testthat::expect_snapshot_error(
    set_soil(soil_data_long, layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T, data_format = list("long", list(param=3, layer="id")))
  )
  soil_data_long_mod <- soil_data_long %>% dplyr::select(-c(id, value))
  testthat::expect_snapshot_error(
    set_soil(soil_data_long_mod, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T)
  )
  soil_data_long_mod <- soil_data_long
  soil_data_long_mod$idd <- soil_data_long_mod$id
  soil_data_long_mod$valuee <- soil_data_long_mod$value
  testthat::expect_snapshot(
    set_soil(soil_data_long_mod, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
             layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg",
             verbose=T)
  )
})

test_that("Soil data units", {
  soil <- set_soil(soil_data_large, id="name", layer_depth=list("epc", "cm"), layer_water_field_cap=list("HCCF", "g/g"),
                   layer_water_wilting_pt=list("HMINF", "g/g"), layer_bulk_density_moist=list("DAF", "g/cm3"),
                   organic_N_conc=list("norg", "g/g"), verbose=T)
  testthat::expect_snapshot(soil)
})

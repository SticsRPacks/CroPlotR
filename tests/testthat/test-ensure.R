context("testing ensure functions")

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
soil_data_wide <- readRDS(file.path(workspace, "soil_data_wide.rds"))
soil <- set_soil(soil_data_wide, id="name", layer_thickness="epc", layer_water_field_cap="HCCF",
                 layer_water_wilting_pt="HMINF", layer_bulk_density="DAF", organic_N_conc="norg")

local_edition(3)
test_that("ensure function", {
  res <- ensure(soil, c("thickness", "soil_max_wtr_cap", "organic_N_conc"))
  testthat::expect_snapshot(str(res))
})

test_that("ensure_wrapper function", {
  soil <- ensure_hardWrapper(soil, c("thickness", "soil_max_wtr_cap", "organic_N_conc"), "thickness.mswc.norg")
  testthat::expect_snapshot(str(soil))
})

test_that("print_missingTree function", {
  lst <- list(A = NA,
              B = list(
                C = list(
                  D = list(
                    E = NA),
                  F = NA,
                  G = list(H = NA, I = NA)
                  )),
              J = list(
                K = NA
              ),
              L = NA,
              N = NA)
  testthat::expect_snapshot(cat(print_missingTree(lst)))
})

test_that("ensure_wrapper function failed", {
  soil <- list(data = data.frame(), data_byLayer = data.frame(), dict = list())
  testthat::expect_snapshot_error(ensure_hardWrapper(soil, c("thickness", "soil_max_wtr_cap", "organic_N_conc"), "thickness.mswc.norg"))
})

test_that("expect_thickness", {
  soil <- list(data = data.frame(id = c("A", "B")), data_byLayer = data.frame(id = c("A","A","B","B"), layer_thickness = 1:4), dict = list())
  testthat::expect_snapshot(ensure_thickness(soil))
})

test_that("ensure_layer_max_wtr_cap", {
  set.seed(0)
  data_byLayer <-
    setNames(
      data.frame(replicate(4, runif(4, min=0, max=10))),
      c("layer_thickness", "layer_bulk_density", "layer_water_field_cap", "layer_water_wilting_pt")
    )
  data_byLayer$id <- c("A","A","B","B")
  soil <- list(data = data.frame(), data_byLayer = data_byLayer, dict = list())
  testthat::expect_snapshot(ensure_layer_max_wtr_cap(soil))
})

test_that("ensure_soil_max_wtr_cap", {
  set.seed(0)
  data_byLayer <-
    setNames(
      data.frame(replicate(1, runif(4, min=0, max=10))),
      c("layer_max_wtr_cap")
    )
  data_byLayer$id <- c("A","A","B","B")
  soil <- list(data = data.frame(id = c("A","B")), data_byLayer = data_byLayer, dict = list())
  testthat::expect_snapshot(ensure_soil_max_wtr_cap(soil))
})

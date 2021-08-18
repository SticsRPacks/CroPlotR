context("testing plot_soil function")

workspace <- system.file(file.path("extdata", "stics_example_input"), package = "CroPlotR")
soil_data_large <- readRDS(file.path(workspace, "soil_data_wide.rds"))
soil <- set_soil(soil_data_large, id="name", layer_depth=list("epc", "cm"), layer_water_field_cap=list("HCCF", "g/g"),
                 layer_water_wilting_pt=list("HMINF", "g/g"), layer_bulk_density_moist=list("DAF", "g/cm^3"),
                 organic_N_conc=list("norg", "g/g"))

test_that("thickness.mswc basic plot",{
  plot <- plot_soil(soil, type="thickness.mswc")
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-thickness.mswc-basic", plot)
  )
})

test_that("thickness.mswc histogram plot",{
  plot <- plot_soil(soil, type="thickness.mswc", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-thickness.mswc-hist", plot)
})

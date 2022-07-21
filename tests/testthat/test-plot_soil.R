context("testing plot_soil function")

workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
soil_data_large <- readRDS(file.path(workspace, "soil_data_wide.rds"))
soil <- set_soil(soil_data_large, id="name", layer_thickness=list("epc", "cm"), layer_water_field_cap=list("HCCF", "g/g"),
                 layer_water_wilting_pt=list("HMINF", "g/g"), layer_bulk_density=list("DAF", "g/cm^3"),
                 organic_N_conc=list("norg", "g/g"))

test_that("thickness.mswc basic plot",{
  plot <- plot_soil(soil, type="thickness.mswc")
  geomTextRepel(plot) <- ggrepel::geom_text_repel(seed = 42)
  testthat::expect_warning(
    vdiffr::expect_doppelganger("plot-thickness.mswc-basic", plot)
  )
})

test_that("thickness.mswc histogram plot",{
  plot <- plot_soil(soil, type="thickness.mswc", histogram=T, interactive = F)
  vdiffr::expect_doppelganger("plot-thickness.mswc-hist", plot)
})


soil_without_epc <- set_soil(soil_data_large, id="name", layer_water_field_cap=list("HCCF", "g/g"),
                 layer_water_wilting_pt=list("HMINF", "g/g"), layer_bulk_density=list("DAF", "g/cm^3"),
                 organic_N_conc=list("norg", "g/g"))

test_that("plot type all warns in case verbose is true and not all graphs can be plotted",{
  expect_message(plot_soil(soil_without_epc, type="all", verbose = TRUE), regexp = "Could not plot thickness.mswc")
  expect_error(plot_soil(soil_without_epc, type="thickness.mswc", verbose = TRUE), regexp = "Graph type `thickness.mswc` requires the following parameters")
})

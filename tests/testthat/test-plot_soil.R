context("testing plot_soil function")

workspace <- system.file(file.path("extdata", "stics_example_input"), package = "CroPlotR")
soil_data_large <- readRDS(file.path(workspace, "soil_data_large.rds"))
soil <- set_soil(soil_data_large, id="name", layer_depth="epc", layer_water_field_cap="HCCF",
                 layer_water_wilting_pt="HMINF", layer_bulk_density_moist="DAF", organic_N_conc="norg")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_sim(workspace = workspace, usm = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm = situations)

test_that("multiplication works", {
  expect_identical(sim, split_df2sim(bind_rows_sim(sim)))
  # hum for obs results can be in different orders ... (since generally different variables are observed for the different situations and bind_rows gather them ...)
  expect_equal(obs$`SC_Pea_2005-2006_N0`, split_df2sim(bind_rows_sim(obs),add_cropr_attr = FALSE)$`SC_Pea_2005-2006_N0`[names(obs$`SC_Pea_2005-2006_N0`)])
  expect_equal(obs$`SC_Wheat_2005-2006_N0`, split_df2sim(bind_rows_sim(obs),add_cropr_attr = FALSE)$`SC_Wheat_2005-2006_N0`[names(obs$`SC_Wheat_2005-2006_N0`)])
  expect_equal(obs$`IC_Wheat_Pea_2005-2006_N0`, split_df2sim(bind_rows_sim(obs),add_cropr_attr = FALSE)$`IC_Wheat_Pea_2005-2006_N0`[names(obs$`IC_Wheat_Pea_2005-2006_N0`)])
})

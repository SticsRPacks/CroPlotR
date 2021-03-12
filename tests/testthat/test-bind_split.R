workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)

test_that("multiplication works", {
  expect_identical(sim, split_df2sim(bind_rows_sim(sim)))
})

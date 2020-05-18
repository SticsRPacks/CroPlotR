context("Ploting")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
plot(sim,obs=obs)

test_that("format of ploting several situations", {
  test_plot= plot(sim,obs=obs)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))
})

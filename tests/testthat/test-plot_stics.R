context("Ploting")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)

test_that("format of ploting several situations", {
  test_plot= plot(sim,obs=obs)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))
})


test_that("Tests with no observations", {
  test_plot= plot(sim)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))

  expect_error(plot(sim,type="scatter"), "No observations found")
  expect_error(plot(sim,plot="obs"), "No observations found")
  expect_error(plot(sim,plot="common"), "No observations found")
})




statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
           formater= format_stics)

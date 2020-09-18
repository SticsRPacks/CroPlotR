context("Ploting")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations, usms_filename = "usms.xml")

# Rotation example
workspace2= system.file(file.path("extdata", "stics_example_successive"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace2,"usms.xml"))
sim_rot= SticsRFiles::get_daily_results(workspace = workspace2, usm_name = situations)


test_that("format of ploting several situations on different graphs", {
  test_plot= plot(sim,obs=obs,all_situations=FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))
})

test_that("format of ploting several situations on a single graph", {
  test_plot= plot(sim,obs=obs,type="scatter",all_situations=TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("all_situations"))
})

test_that("Tests with no observations", {
  test_plot= plot(sim, all_situations=FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))

  expect_error(plot(sim,type="scatter",force=FALSE), "No observations found")
  expect_error(plot(sim,select_dyn="obs",force=FALSE), "No observations found")
  expect_error(plot(sim,select_dyn="common",force=FALSE), "No observations found")
  expect_error(plot(sim,select_scat="res",force=FALSE), "No observations found")
})

test_that("Extract plot of one situation", {
  test_plot= extract_plot(plot(sim,obs=obs,type="scatter",all_situations=FALSE),
                          situations=c("IC_Wheat_Pea_2005-2006_N0"))
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("IC_Wheat_Pea_2005-2006_N0"))

  expect_error(extract_plot(plot(sim,obs=obs,type="scatter",all_situations=TRUE),
                          situations=c("IC_Wheat_Pea_2005-2006_N0"),force=FALSE),
               "Impossible to extract situations from a list of a single ggplot covering all situations")
})

test_that("Extract plots of one variable", {
  test_plot= extract_plot(plot(sim,obs=obs,type="scatter",all_situations=FALSE),
                          var=c("lai_n"))
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_equal(names(test_plot),
               c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0"))
})



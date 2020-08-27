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

test_that("dynamic plots", {
  # Each dynamic plot is stored as a list in "test_plots_dyn.RData"
  load(file.path(workspace,"test_plots_dyn.RData"))

  # Classic dynamic plot
  expect_equal(dyn$data, plot(sim,obs=obs)$data)
  expect_equal(dyn$layers, plot(sim,obs=obs)$layers)

  # Rotation parameter
  list_rot = plot(sim_rot, var = c("resmes","CNgrain"), successive = list(list("demo_BareSoil2","demo_maize3")))
  expect_equal(rot$data, list_rot$data)
  expect_equal(rot$layers, list_rot$layers)

  # Overlap parameter
  expect_equal(overlay$data, plot(sim, obs= obs, overlap = list(list("lai_n","masec_n")))$data)
  expect_equal(overlay$layers, plot(sim, obs= obs, overlap = list(list("lai_n","masec_n")))$layers)

  # Rotation and overlap parameters together
  all_in_one = plot(sim_rot, obs= obs, successive = list(list("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")),
                    overlap = list(list("lai_n","masec_n")))
  expect_equal(all$data, all_in_one$data)
  expect_equal(all$layers, all_in_one$layers)
})

test_that("scatter plots", {
  # all_situations = FALSE
  expect_known_hash(plot(sim, obs = obs, type = "scatter", all_situations = FALSE)$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter", all_situations = FALSE)$layers, hash = "f9e884084b")

  # all_situations = TRUE
  expect_known_hash(plot(sim, obs = obs, type = "scatter")$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter")$layers, hash = "f9e884084b")

  # Residual
  expect_known_hash(plot(sim, obs = obs, type = "scatter", select_scat = "res")$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter", select_scat = "res")$layers, hash = "f9e884084b")

  # reference_var parameter
  expect_known_hash(plot(sim, obs = obs, type = "scatter", select_scat = "res", reference_var = "lai_n_sim")$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter", select_scat = "res", reference_var = "lai_n_sim")$layers, hash = "f9e884084b")

  # Text shape
  expect_known_hash(plot(sim, obs = obs, type = "scatter", shape_sit = "txt")$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter", shape_sit = "txt")$layers, hash = "f9e884084b")

  # Symbol shape
  expect_known_hash(plot(sim, obs = obs, type = "scatter", shape_sit = "symbol")$data, hash = "f9e884084b")
  expect_known_hash(plot(sim, obs = obs, type="scatter", shape_sit = "symbol")$layers, hash = "f9e884084b")

  # Group of situations
  group_sit = plot(sim, obs= obs, type = "scatter", all_situations = TRUE,
       shape_sit = "group", situation_group = list(list("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")))
  expect_known_hash(group_sit$data, hash = "f9e884084b")
  expect_known_hash(group_sit$layers, hash = "f9e884084b")
})



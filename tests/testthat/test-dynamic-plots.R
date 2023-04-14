
# # Make the reference data:
#
# workspace <- system.file(file.path("extdata", "stics_example_1"),
#                          package = "CroPlotR"
# )
#
# situations <- SticsRFiles::get_usms_list(
#   file = file.path(workspace,"usms.xml")
# )
#
# sim <- SticsRFiles::get_sim(
#   workspace = workspace,
#   usm = situations,
#   usms_file = file.path(workspace,"usms.xml")
# )
#
# obs <- SticsRFiles::get_obs(
#   workspace = workspace,
#   usm = situations,
#   usms_file = file.path(workspace, "usms.xml")
# )
#
# # Rotation example
# workspace2 <- system.file(
#   file.path("extdata", "stics_example_successive"),
#   package = "CroPlotR"
# )
# situations <- SticsRFiles::get_usms_list(
#   file = file.path(workspace2, "usms.xml")
# )
# sim_rot <- SticsRFiles::get_sim(
#   workspace = workspace2,
#   usm = situations,
#   usms_file = file.path(workspace2, "usms.xml")
# )
#
# workspace2 <- system.file(file.path("extdata", "stics_example_2"),
#  package = "CroPlotR")
# sim2 <- SticsRFiles::get_sim(
#   workspace = workspace2,
#   usms_file = file.path(workspace2,"usms.xml")
# )

# save(sim, sim2, obs, sim_rot, file = "tests/testthat/_inputs/sim_obs.RData")

# Loading the inputs
load("_inputs/sim_obs.RData")

test_that("format of plotting several situations on different graphs", {
  test_plot <- plot(sim, obs = obs, all_situations = FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_true(all(names(test_plot) %in%
                    c(
                      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0"
                    )))
})

test_that("Tests with no observations", {
  test_plot <- plot(sim, all_situations = FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_true(all(names(test_plot) %in%
                    c(
                      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0"
                    )))
  expect_error(plot(sim, select_dyn = "obs", force = FALSE),
               "No observations found")
  expect_error(
    plot(sim, select_dyn = "common", force = FALSE),
    "No observations found")
})

test_that("Test plot overlap", {
  test_plot <- plot(sim, obs = obs, overlap = list(list("lai_n", "masec_n")))

  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$shape, "Variable")
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$colour, "Variable")
  expect_equal(
    unique(test_plot$`SC_Pea_2005-2006_N0`$data$group_var),
    "lai_n | masec_n"
  )
  expect_equal(
    unique(test_plot$`SC_Pea_2005-2006_N0`$data$variable),
    c("lai_n", "masec_n")
  )

  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$group_var),
    "lai_n | masec_n"
  )
  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$variable),
    c("lai_n", "masec_n")
  )
})

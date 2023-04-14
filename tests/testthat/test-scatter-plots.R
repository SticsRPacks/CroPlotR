
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

test_that("Tests with no observations", {
  expect_error(plot(sim, type = "scatter", force = FALSE),
               "No observations found")
  expect_error(plot(sim, select_scat = "res", force = FALSE),
               "No observations found")
})

test_that("Extract plot of one situation", {
  test_plot <- extract_plot(plot(sim,
                                 obs = obs, type = "scatter",
                                 all_situations = FALSE
  ),
  situation = c("IC_Wheat_Pea_2005-2006_N0")
  )
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("IC_Wheat_Pea_2005-2006_N0"))

  expect_error(
    extract_plot(plot(sim,
                      obs = obs, type = "scatter",
                      all_situations = TRUE
    ),
    situation = c("IC_Wheat_Pea_2005-2006_N0"),
    force = FALSE
    ),
    "Impossible to extract situations from a list of a single ggplot covering all situations"
  )
})

test_that("Extract plots of one variable", {
  test_plot <- extract_plot(plot(sim,
                                 obs = obs, type = "scatter",
                                 all_situations = FALSE
  ),
  var = c("lai_n")
  )
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_true(all(names(test_plot) %in%
                    c(
                      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0"
                    )))
})


# Test labels of ggplot in function of the case (see doc/aesthetics_scatter.xlsx)
sim_mixture <- sim
sim_sole_crop <- sim[c("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")]

## mixture & plot_per_sit => col=Plant ; group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_mixture, obs = obs, type = "scatter",
                    all_situations = FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), length(sim_mixture))
  expect_equal(names(test_plot), names(sim_mixture))
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$col, "Plant")
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, NULL)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$linetype, NULL)
#  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group, "Situation")
})

## mixture & txt => col=Plant ; group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_mixture, obs = obs, type = "scatter",
                    all_situations = TRUE, shape_sit = "txt")
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), "all_situations")
  expect_equal(test_plot$all_situations$labels$col, "Plant")
  expect_equal(test_plot$all_situations$labels$shape, NULL)
  expect_equal(test_plot$all_situations$labels$linetype, NULL)
  #  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group, "Situation")
})

## mixture & none => col=Plant ; group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_mixture, obs = obs, type = "scatter",
                    all_situations = TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), "all_situations")
  expect_equal(test_plot$all_situations$labels$col, "Plant")
  expect_equal(test_plot$all_situations$labels$shape, NULL)
  expect_equal(test_plot$all_situations$labels$linetype, NULL)
  #  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group, "Situation")
})

## mixture & !all_situation & !successive => col=Plant ; group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_mixture, obs = obs, type = "scatter",
                    all_situations = FALSE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), length(test_plot))
  expect_equal(names(test_plot), names(test_plot))
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$col, "Plant")
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, NULL)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$linetype, NULL)
  #  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group, "Situation")
})

## => traiter colonnes O/P/Q/R
# ...



## several_sit => group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_sole_crop, obs = obs, type = "scatter",
                    all_situations = TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("all_situations"))
  expect_equal(test_plot$all_situations$labels$col, NULL)
  expect_equal(test_plot$all_situations$labels$shape, NULL)
  expect_equal(test_plot$all_situations$labels$linetype, NULL)
#  expect_equal(test_plot$all_situations$labels$group, "Situation")
})

## several_sit & (symbol | group) => col=Situation ; group=situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim_sole_crop, obs = obs, type = "scatter", shape_sit="symbol",
                    all_situations = TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("all_situations"))
  expect_equal(test_plot$all_situations$labels$col, "Situation")
  expect_equal(test_plot$all_situations$labels$group, "Situation")
})

## mixture & several_sit & (symbol | group) => col=Plant ; shape=Situation ; group=Situation
test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim, obs = obs, type = "scatter", shape_sit="symbol",
                    all_situations = TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("all_situations"))
  expect_equal(test_plot$all_situations$labels$col, "Plant")
  expect_equal(test_plot$all_situations$labels$shape, "Situation")
  expect_equal(test_plot$all_situations$labels$group, "Situation")
})


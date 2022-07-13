context("plotting")

workspace <- system.file(file.path("extdata", "stics_example_1"),
  package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(file = file.path(workspace,
                                                          "usms.xml"))
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)
obs <- SticsRFiles::get_obs(workspace = workspace, usm = situations)

# Rotation example
workspace2 <- system.file(file.path("extdata", "stics_example_successive"),
  package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(file = file.path(workspace2,
                                                          "usms.xml"))
sim_rot <- SticsRFiles::get_sim(workspace = workspace2, usm = situations)


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

test_that("format of plotting several situations on a single graph", {
  test_plot <- plot(sim, obs = obs, type = "scatter", all_situations = TRUE)
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 1)
  expect_equal(names(test_plot), c("all_situations"))
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

  expect_error(plot(sim, type = "scatter", force = FALSE),
               "No observations found")
  expect_error(plot(sim, select_dyn = "obs", force = FALSE),
               "No observations found")
  expect_error(
    plot(sim, select_dyn = "common", force = FALSE),
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
    c("plant_1", "plant_2")
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


# options(stringsAsFactors=FALSE)

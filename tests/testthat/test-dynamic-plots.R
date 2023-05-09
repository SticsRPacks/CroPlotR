
library(CroPlotR)
library(testthat)

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

# setwd("tests/testthat") (local test)
load("_inputs/sim_obs.RData")

#sim_sole_crop <- sim[c("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")]
#sim_mixture <- sim[c("IC_Wheat_Pea_2005-2006_N0")]

#sim_sole_crop_v2 = sim_sole_crop
#sim_sole_crop_v2$`SC_Pea_2005-2006_N0`[3] <- sim_sole_crop$`SC_Pea_2005-2006_N0`[3]*2
#sim_sole_crop_v2$`SC_Pea_2005-2006_N0`[2] <- sim_sole_crop$`SC_Pea_2005-2006_N0`[2]*2
#sim_sole_crop_v2$`SC_Wheat_2005-2006_N0`[3] <- sim_sole_crop$`SC_Wheat_2005-2006_N0`[3]*2
#sim_sole_crop_v2$`SC_Wheat_2005-2006_N0`[2] <- sim_sole_crop$`SC_Wheat_2005-2006_N0`[2]*2

#sim_mixture_v2 = sim_mixture
#sim_mixture_v2$`IC_Wheat_Pea_2005-2006_N0`[3] <- sim_mixture$`IC_Wheat_Pea_2005-2006_N0`[3]*2
#sim_mixture_v2 $`IC_Wheat_Pea_2005-2006_N0`[2] <- sim_mixture$`IC_Wheat_Pea_2005-2006_N0`[2]*2


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


all_plots <- list()

### only overlap

test_that("Test plot only overlap", {
  test_plot <- plot(sim_sole_crop, obs = obs, overlap = list(list("lai_n", "masec_n")), title ="Test plot only overlap" )
  all_plots <<- c(all_plots, test_plot)
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
})

### only mixture

test_that("Test plot only mixture", {
  test_plot <- plot(sim_mixture, obs = obs, title = "Test plot only mixture" )
  all_plots <<- c(all_plots, test_plot)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, "Plant")
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$colour, "Plant")
  expect_equal(grepl("Plant", test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group), TRUE)

})


### only version

test_that("Test plot only version", {
  test_plot <- plot(sim_sole_crop,sim2_sole_crop, obs=obs, title ="Test plot only version" )
  all_plots <<- c(all_plots, test_plot)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$shape, NULL)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$colour, NULL)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$group, "group")
  expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(x) grepl("Version_", rlang::eval_tidy(x$mapping$colour)))), TRUE)


  expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`[sapply(test_plot$`SC_Pea_2005-2006_N0`, function(y) "shape" %in% attributes(y$mapping)$names)] ,
                          function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
  )
  ), TRUE)
})


### overlap + mixture

test_that("Test plot overlap + mixture", {
  test_plot <- plot(sim_mixture, obs = obs, overlap = list(list("lai_n", "masec_n")), title ="Test plot overlap + mixture" )
  all_plots <<- c(all_plots, test_plot)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, "Variable")
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$colour, "Plant")
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$linetype, "Variable")
  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$group_var),
    "lai_n | masec_n"
  )
  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$variable),
    c("lai_n", "masec_n")
  )
  expect_equal(
    unique(test_plot$`IC_Wheat_Pea_2005-2006_N0`$data$Plant),
    c("ble", "poi")
  )
})

### overlap + version

test_that("Test plot overlap + version", {
  test_plot <- plot(sim_sole_crop,sim2_sole_crop, obs = obs, overlap = list(list("lai_n", "masec_n")), title="Test plot overlap + version")
  all_plots <<- c(all_plots, test_plot)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$shape, NULL)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$colour, "Variable")
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$linetype, NULL)
  expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$group, "variable")

  expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(x) grepl("variable", rlang::as_label(x$mapping$colour)))), TRUE)
  expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers[sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(y) "shape" %in% attributes(y$mapping)$names)] ,
                          function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
  )
  ), TRUE)
  expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers[sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(y) "linetype" %in% attributes(y$mapping)$names)] ,
                          function(x) grepl("Version_", rlang::eval_tidy(x$mapping$linetype))
  )
  ), TRUE)


  expect_equal(
    unique(test_plot$`SC_Pea_2005-2006_N0`$data$group_var),
    "lai_n | masec_n"
  )
  expect_equal(
    unique(test_plot$`SC_Pea_2005-2006_N0`$data$variable),
    c("lai_n", "masec_n")
  )
  expect_equal(
    unique(test_plot$`SC_Pea_2005-2006_N0`$data$Sit_Name),
    c("SC_Pea_2005-2006_N0")
  )
})



### mixture + version


test_that("Test plot mixture + version", {
  test_plot <- plot(sim_mixture,sim2_mixture, obs = obs,title = "Test plot mixture + version")
  all_plots <<- c(all_plots, test_plot)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, NULL)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$colour, NULL)
  expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$linetype, NULL)
  expect_equal(grepl("Plant",test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group), TRUE)

  expect_equal(all(sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(x) grepl("Plant",rlang::as_label(x$mapping$colour)))), TRUE)
  expect_equal(all(sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers[sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(y) "shape" %in% attributes(y$mapping)$names)] ,
                          function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
  )
  ), TRUE)
  expect_equal(all(sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers[sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(y) "linetype" %in% attributes(y$mapping)$names)] ,
                          function(x) grepl("Version_", rlang::eval_tidy(x$mapping$linetype))
  )
  ), TRUE)

})

if (!testthat:::on_ci()) {
  save_plot_pdf(all_plots,out_dir = getwd(),file_name = "all_plots_dynamic")
}


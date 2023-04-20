
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

## Define all the cases useful for the tests
sim_mixture <- sim
sim2_mixture <- sim
for (sit in names(sim2_mixture)) {
  sim2_mixture[[sit]][,c("lai_n","masec_n")]<-sim2_mixture[[sit]][,c("lai_n","masec_n")]*1.1
}
sim_sole_crop <- sim[c("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")]
sim2_sole_crop <- sim_sole_crop
for (sit in names(sim2_sole_crop)) {
  sim2_sole_crop[[sit]][,c("lai_n","masec_n")]<-sim2_sole_crop[[sit]][,c("lai_n","masec_n")]*1.1
}

## Read the file describing the configurations and results of the tests
tmp <- read.csv(file="_inputs/test_plot.csv",header = TRUE, sep = ";", stringsAsFactors = FALSE)

##
tmp$sim <- lapply(tmp$mixture, function(x) if (x) sim_mixture else sim_sole_crop)
tmp$sim2 <- lapply(1:nrow(tmp), function(i) {
  if (tmp$version[i] & tmp$mixture[i]) {
    sim2_mixture
  } else if (tmp$version[i] & !tmp$mixture[i]) {
    sim2_sole_crop
  } else {
    NULL
  }
})
tmp$length <- lapply(1:nrow(tmp), function(i) if (tmp$all_situations[i]) 1 else length(tmp$sim[[i]]))
tmp$name <- lapply(1:nrow(tmp), function(i) if (tmp$all_situations[i]) "all_situations" else names(tmp$sim[[i]]))
tmp$situation_group <- lapply(1:nrow(tmp), function(i) if (tmp$shape_sit[i]=="group") list(as.list(names(tmp$sim[[i]]))) else NULL)

invisible(lapply(1:nrow(tmp), function(i) {
  test_that(paste0("Test #",i), {
    if (tmp$version[i]) {
      test_plot <- plot(tmp$sim[[i]], tmp$sim2[[i]], obs = obs, type = "scatter",
                        all_situations = tmp$all_situations[i])
    } else {
      test_plot <- plot(tmp$sim[[i]], obs = obs, type = "scatter",
                        all_situations = tmp$all_situations[i])
    }
    expect_true(is.list(test_plot))
    expect_equal(length(test_plot), tmp$length[[i]])
    expect_equal(names(test_plot), tmp$name[[i]])
    col <- if (tmp$col[i]=="NULL") NULL else tmp$col[i]
    shape <- if (tmp$shape[i]=="NULL") NULL else tmp$shape[i]
    linetype <- if (tmp$linetype[i]=="NULL") NULL else tmp$linetype[i]
    group <- if (tmp$group[i]=="NULL") NULL else tmp$group[i]
    expect_equal(test_plot[[1]]$labels$col, col)
    expect_equal(test_plot[[1]]$labels$shape, shape)
    expect_equal(test_plot[[1]]$labels$linetype, linetype)
    #  expect_equal(x$labels$group, group)
  })
}))

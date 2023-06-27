# /!\ This file is not used anymore, see vdiffr folder instead



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
# load("_inputs/sim_obs.RData")

# Test that plots remain the same (based on the readme) -------------------

# NB: use this command for reviewing changes: testthat::snapshot_review()

# These tests are done manually only using testthat::test_file("tests/testthat/test-plot-snap.R")
# interactive tests will lead to error message: "Can't compare snapshot to reference when testing interactively."

# These tests need R version >= 4.2 for testhat >= 3.0.0.
# Also, we only test on the OS and R version the snapshots were built on.
# if (getRversion() >= "4.2.1") {
#   set.seed(1)

#   # Figure 1 ----------------------------------------------------------------

#   p <- plot(sim, obs = obs)

#   test_that("Build figure 1 (simple) IC", {
#     vdiffr::expect_doppelganger(
#       "fig.1_simple_dynamic_IC",
#       p$`IC_Wheat_Pea_2005-2006_N0`
#     )
#   })

#   test_that("Build figure 1 (simple) Pea", {
#     vdiffr::expect_doppelganger(
#       "fig.1_simple_dynamic_Pea",
#       p$`SC_Pea_2005-2006_N0`
#     )
#   })

#   test_that("Build figure 1 (simple) Wheat", {
#     vdiffr::expect_doppelganger(
#       "fig.1_simple_dynamic_Wheat",
#       p$`SC_Wheat_2005-2006_N0`
#     )
#   })

#   # Figure 2 ----------------------------------------------------------------

#   # workspace <- system.file(
#   #   file.path("extdata", "stics_example_successive"),
#   #   package = "CroPlotR"
#   # )
#   # situations <- SticsRFiles::get_usms_list(
#   #   file = file.path(workspace,"usms.xml")
#   # )
#   # sim_rot <- SticsRFiles::get_sim(
#   #   workspace = workspace,
#   #   usm = situations,
#   #   usms_file = file.path(workspace, "usms.xml")
#   # )

#   test_that("Build figure 2 (successive)", {
#     vdiffr::expect_doppelganger(
#       "fig.2_successive",
#       suppressWarnings(
#         # suppressWarnings because there is one warning I can't get rid off yet
#         plot(
#           sim_rot,
#           var = c("resmes", "masec_n"),
#           successive = list(list("demo_Wheat1", "demo_BareSoil2", "demo_maize3"))
#         )
#       )
#     )
#   })

#   # Figure 3 ----------------------------------------------------------------

#   test_that("Build figure 3 (variables overlap)", {
#     p <- plot(sim, obs = obs, overlap = list(list("lai_n", "masec_n")))

#     vdiffr::expect_doppelganger(
#       "fig.3_overlap_variables_IC",
#       p$`IC_Wheat_Pea_2005-2006_N0`
#     )

#     vdiffr::expect_doppelganger(
#       "fig.3_overlap_variables_Pea",
#       p$`SC_Pea_2005-2006_N0`
#     )

#     vdiffr::expect_doppelganger(
#       "fig.3_overlap_variables_Wheat",
#       p$`SC_Wheat_2005-2006_N0`
#     )
#   })

#   # Figure 4 ----------------------------------------------------------------

#   test_that("Build figure 4 (scatter allsit FALSE)", {
#     vdiffr::expect_doppelganger(
#       "fig.4_scatter_all_sitFALSE",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = FALSE
#       )$`IC_Wheat_Pea_2005-2006_N0`
#     )
#   })

#   # Figure 5 ----------------------------------------------------------------

#   test_that("Build figure 5 (residues)", {
#     vdiffr::expect_doppelganger(
#       "fig.4_scatter_residues_sitFALSE",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = FALSE
#       )$`IC_Wheat_Pea_2005-2006_N0`
#     )
#   })

#   # Figure 6 ----------------------------------------------------------------

#   test_that("Build figure 6 (scatter allsit TRUE)", {
#     vdiffr::expect_doppelganger(
#       "fig.6_scatter_all_sitTRUE",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = TRUE
#       )$all_situations
#     )
#   })

#   # Figure 7 ----------------------------------------------------------------

#   test_that("Build figure 7 (res + lai x var + allsit TRUE)", {
#     vdiffr::expect_doppelganger(
#       "fig.7_res_xvar_lai",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         select_scat = "res",
#         all_situations = TRUE,
#         reference_var = "lai_n_sim"
#       )$all_situations
#     )
#   })


#   # Figure 8 ----------------------------------------------------------------
#   set.seed(1)

#   test_that("Build figure 8 (scatter txt shape)", {
#     # set.seed(1) # because ggrepel used random sampling I believe
#     vdiffr::expect_doppelganger(
#       "fig.8_scatter_txt_shape",
#       plot(
#         sim,
#         obs = obs[c(2, 3)],
#         type = "scatter",
#         all_situations = TRUE,
#         shape_sit = "txt"
#       )$all_situations
#     )
#   })

#   # Figure 9 ----------------------------------------------------------------

#   test_that("Build figure 9 (scatter symbol shape)", {
#     vdiffr::expect_doppelganger(
#       "fig.9_scatter_symbol_shape",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = TRUE,
#         shape_sit = "symbol"
#       )$all_situations
#     )
#   })

#   # Figure 10 ----------------------------------------------------------------

#   test_that("Build figure 10 (scatter grouped shape)", {
#     vdiffr::expect_doppelganger(
#       "fig.10_scatter_symbol_grouped_shape_1",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = TRUE,
#         shape_sit = "group",
#         situation_group = list(
#           list("SC_Pea_2005-2006_N0", "SC_Wheat_2005-2006_N0")
#         )
#       )$all_situations
#     )

#     vdiffr::expect_doppelganger(
#       "fig.10_scatter_symbol_grouped_shape_2",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = TRUE,
#         shape_sit = "group",
#         situation_group = list(
#           "Two Single Crops" = list("SC_Pea_2005-2006_N0", "SC_Wheat_2005-2006_N0")
#         )
#       )$all_situations
#     )
#   })


#   # Figure 11 ----------------------------------------------------------------

#   test_that("Build figure 11 (scatter filter var)", {
#     vdiffr::expect_doppelganger(
#       "fig.11_scatter_filter_var",
#       plot(
#         sim,
#         obs = obs,
#         type = "scatter",
#         all_situations = TRUE,
#         var = c("lai_n")
#       )$all_situations
#     )
#   })


#   # Figure 12 ----------------------------------------------------------------

#   test_that("Build figure 12 (scatter error bars)", {
#     obs_sd <- obs
#     obs_sd$`SC_Pea_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
#       c("Date", "Plant"))] <- 0.05 *
#       obs_sd$`SC_Pea_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
#         c("Date", "Plant"))]
#     obs_sd$`SC_Wheat_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
#       c("Date", "Plant"))] <- 0.2 *
#       obs_sd$`SC_Wheat_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
#         c("Date", "Plant"))]

#     vdiffr::expect_doppelganger(
#       "fig.12_scatter_error_bars",
#       plot(
#         sim,
#         obs = obs,
#         obs_sd = obs_sd,
#         type = "scatter",
#         all_situations = TRUE
#       )$all_situations
#     )
#   })

#   # Figure 13 ----------------------------------------------------------------

#   test_that("Build figure 13 (group comparison)", {
#     vdiffr::expect_doppelganger(
#       "fig.13_group_comparison_dynamic",
#       plot(sim, sim2, obs = obs, all_situations = FALSE)
#     )

#     vdiffr::expect_doppelganger(
#       "fig.13_group_comparison_scatter",
#       plot(
#         "New version" = sim, original = sim2, obs = obs,
#         type = "scatter", all_situations = FALSE
#       )
#     )
#   })
# } # Add tests on stats plots here.

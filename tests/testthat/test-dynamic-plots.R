#
# Tests the scatter plots
#
# Automatic tests + generates a pdf and svg in _outputs to visually check the plots
#
# See doc on tests for CroPlotR in doc/doc_on_tests.md

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

# In case of local tests, plots are stored on _outputs folder
if (!testthat:::on_ci()) {
  tmpdir <- "_outputs"
  if (!file.exists(tmpdir)) {
    dir.create(tmpdir)
  }
}

# Function for making snapshot for vdiffr tests

make_snapshot <- function(name, plot, tmpdir) {
  if (is.null(tmpdir)) {
    return()
  }

  # From https://github.com/r-lib/vdiffr/blob/main/R/expect-doppelganger.R
  testthat::local_edition(3)
  fig_name <- vdiffr:::str_standardise(name)
  file <- file.path(tmpdir, paste0(fig_name, ".svg"))

  print(paste("Making snapshot", name, "and saving in", file))

  vdiffr:::write_svg(plot, file, name)

  return(file)
}

if (!exists("pkg_version")) {
  pkg_version <- "Test"
}

if (!exists("tmpdir")) {
  tmpdir <- tempdir()
  print(paste(
    "Temporary folder path not defined before running this script ",
    "('tmpdir' object not existing) => snapshots will be saved in.",
    tmpdir
  ))
} else {
  print(paste("Saving snapshots in", tmpdir))
}

pkg_version <- paste0("_", pkg_version)

print(paste("Script called from", getwd()))

prefix <- "dynamic"

# Set seed for comparison of graphs (some use random process)
set.seed(1)

# Run the tests and generate snapshots

test_that("format of plotting several situations on different graphs", {
  test_plot <- plot(sim, obs = obs, all_situations = FALSE)

  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"several situations on different graphs\" not yet implemented (plot return NA)")

  } else {

    expect_true(is.list(test_plot))
    expect_equal(length(test_plot), 3)
    expect_true(all(names(test_plot) %in%
                      c(
                        "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
                        "SC_Wheat_2005-2006_N0"
                      )))

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.1_simple_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })

  }
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
  expect_error(
    plot(sim, select_dyn = "obs", force = FALSE),
    "No observations found"
  )
  expect_error(
    plot(sim, select_dyn = "common", force = FALSE),
    "No observations found"
  )

  lapply(names(test_plot), function(x) {
    make_snapshot(
      paste0(prefix, "_fig.2_no_obs_", x, pkg_version),
      test_plot[[x]],
      tmpdir
    )
  })
})


all_plots <- list()

### only overlap

test_that("Test plot only overlap", {
  test_plot <- plot(sim_sole_crop, obs = obs, overlap = list(list("lai_n", "masec_n")), title = "Test plot only overlap")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"only overlap\" not yet implemented (plot return NA)")

  } else {

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

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.3_overlap_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})

### only mixture

test_that("Test plot only mixture", {
  test_plot <- plot(sim_mixture, obs = obs, title = "Test plot only mixture")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"only mixture\" not yet implemented (plot return NA)")

  } else {

    all_plots <<- c(all_plots, test_plot)
    expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, "Plant")
    expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$colour, "Plant")
    expect_equal(grepl("Plant", test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group), TRUE)

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.4_mixture_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})


### only version

test_that("Test plot only version", {
  test_plot <- plot(sim_sole_crop, sim2_sole_crop, obs = obs, title = "Test plot only version")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"only version\" not yet implemented (plot return NA)")

  } else {

    all_plots <<- c(all_plots, test_plot)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$shape, NULL)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$colour, NULL)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$group, "group")
    expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(x) grepl("Version_", rlang::eval_tidy(x$mapping$colour)))), TRUE)


    expect_equal(all(sapply(
      test_plot$`SC_Pea_2005-2006_N0`[sapply(test_plot$`SC_Pea_2005-2006_N0`, function(y) "shape" %in% attributes(y$mapping)$names)],
      function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
    )), TRUE)

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.5_version_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})


### overlap + mixture

test_that("Test plot overlap + mixture", {
  test_plot <- plot(sim_mixture, obs = obs, overlap = list(list("lai_n", "masec_n")), title = "Test plot overlap + mixture")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"overlap+mixture\" not yet implemented (plot return NA)")

  } else {

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

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.6_overlap_mixture_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})

### overlap + version

test_that("Test plot overlap + version", {
  test_plot <- plot(sim_sole_crop, sim2_sole_crop, obs = obs, overlap = list(list("lai_n", "masec_n")), title = "Test plot overlap + version")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"overlap+version\" not yet implemented (plot return NA)")

  } else {

    all_plots <<- c(all_plots, test_plot)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$shape, NULL)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$colour, "Variable")
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$linetype, NULL)
    expect_equal(test_plot$`SC_Pea_2005-2006_N0`$labels$group, "variable")

    expect_equal(all(sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(x) grepl("variable", rlang::as_label(x$mapping$colour)))), TRUE)
    expect_equal(all(sapply(
      test_plot$`SC_Pea_2005-2006_N0`$layers[sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(y) "shape" %in% attributes(y$mapping)$names)],
      function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
    )), TRUE)
    expect_equal(all(sapply(
      test_plot$`SC_Pea_2005-2006_N0`$layers[sapply(test_plot$`SC_Pea_2005-2006_N0`$layers, function(y) "linetype" %in% attributes(y$mapping)$names)],
      function(x) grepl("Version_", rlang::eval_tidy(x$mapping$linetype))
    )), TRUE)


    expect_equal(
      unique(test_plot$`SC_Pea_2005-2006_N0`$data$group_var),
      "lai_n | masec_n"
    )
    expect_equal(
      unique(test_plot$`SC_Pea_2005-2006_N0`$data$variable),
      c("lai_n", "masec_n")
    )
    expect_equal(
      unique(test_plot$`SC_Pea_2005-2006_N0`$data$sit_name),
      c("SC_Pea_2005-2006_N0")
    )

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.7_overlap_version_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})



### mixture + version


test_that("Test plot mixture + version", {
  test_plot <- plot(sim_mixture, sim2_mixture, obs = obs, title = "Test plot mixture + version")
  if (any(is.na(test_plot))) {

    warning("Dynamic Plot \"mixture+version\" not yet implemented (plot return NA)")

  } else {

    all_plots <<- c(all_plots, test_plot)
    expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$shape, NULL)
    expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$colour, NULL)
    expect_equal(test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$linetype, NULL)
    expect_equal(grepl("Plant", test_plot$`IC_Wheat_Pea_2005-2006_N0`$labels$group), TRUE)

    expect_equal(all(sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(x) grepl("Plant", rlang::as_label(x$mapping$colour)))), TRUE)
    expect_equal(all(sapply(
      test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers[sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(y) "shape" %in% attributes(y$mapping)$names)],
      function(x) grepl("Version_", rlang::eval_tidy(x$mapping$shape))
    )), TRUE)
    expect_equal(all(sapply(
      test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers[sapply(test_plot$`IC_Wheat_Pea_2005-2006_N0`$layers, function(y) "linetype" %in% attributes(y$mapping)$names)],
      function(x) grepl("Version_", rlang::eval_tidy(x$mapping$linetype))
    )), TRUE)

    lapply(names(test_plot), function(x) {
      make_snapshot(
        paste0(prefix, "_fig.7_mixture_version_", x, pkg_version),
        test_plot[[x]],
        tmpdir
      )
    })
  }
})

if (!testthat:::on_ci()) {
  save_plot_pdf(all_plots, out_dir = tmpdir, file_name = "all_plots_dynamic")
  print(paste("Plots saved in pdf format in ", tmpdir))
}

#
# Tests the scatter plots
#
# Automatic tests + generates a pdf and svg in folder _outputs to visually check the plots
# All combinations of plots and expected values of the tests are described in
# the file _inputs/tests_scatter_plot.csv
#
# See doc on tests for CroPlotR in doc/doc_on_tests.md
#


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

## Define all the cases useful for the tests
# sim_mixture <- sim
# sim2_mixture <- sim
# for (sit in names(sim2_mixture)) {
#   sim2_mixture[[sit]][,c("lai_n","masec_n")]<-sim2_mixture[[sit]][,c("lai_n","masec_n")]*1.1
# }
# sim_sole_crop <- sim[c("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")]
# sim2_sole_crop <- sim_sole_crop
# for (sit in names(sim2_sole_crop)) {
#   sim2_sole_crop[[sit]][,c("lai_n","masec_n")]<-sim2_sole_crop[[sit]][,c("lai_n","masec_n")]*1.1
# }

# save(sim, sim2, sim_mixture, sim2_mixture, sim_sole_crop, sim2_sole_crop, obs, sim_rot, file = "tests/testthat/_inputs/sim_obs.RData")

# Loading the inputs
# setwd("tests/testthat") # (local test)
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

prefix <- "scatter"

# Set seed for comparison of graphs (some use random process)
set.seed(1)

# Run the tests and generate snapshots

test_that("Tests with no observations", {
  expect_error(plot(sim, type = "scatter", force = FALSE),
               "Observations are required but not provided")
  expect_error(plot(sim, type = "scatter", select_scat = "res", force = FALSE),
               "Observations are required but not provided")
})


test_that("Extract plot of one situation", {

  p <- plot(sim,
       obs = obs, type = "scatter",
       all_situations = FALSE
  )

  if (any(is.na(p))) {

    message(paste("Scatter plot all_sit=FALSE not yet implemented (plot return NA)"))

  } else {

    test_plot <- extract_plot(p,
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
  }
})

test_that("Extract plots of one variable", {
  p <- plot(sim,
            obs = obs, type = "scatter",
            all_situations = FALSE
  )

  if (any(is.na(p))) {

    message(paste("Scatter plot all_sit=FALSE not yet implemented (plot return NA)"))

  } else {

    test_plot <- extract_plot(p,
  var = c("lai_n")
  )
  expect_true(is.list(test_plot))
  expect_equal(length(test_plot), 3)
  expect_true(all(names(test_plot) %in%
                    c(
                      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
                      "SC_Wheat_2005-2006_N0"
                    )))
  }
})


# Test labels of ggplot in function of the case (see doc/aesthetics_scatter.xlsx)

## Read the file describing the configurations and results of the tests
tmp <- read.csv(file="_inputs/tests_scatter_plots.csv",
                header = TRUE, sep = ";", stringsAsFactors = FALSE)

## Set sim and sim2 depending on mixture or not
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

# Compute length and names of the returned plot, and situation_group
tmp$length <- lapply(1:nrow(tmp),
                     function(i) if (tmp$all_situations[i]) 1 else length(tmp$sim[[i]]))
tmp$name <- lapply(1:nrow(tmp),
                   function(i) if (tmp$all_situations[i]) "all_situations" else names(tmp$sim[[i]]))
tmp$situation_group <- lapply(1:nrow(tmp),
                              function(i) if (tmp$shape_sit[i]=="group") list(as.list(head(names(tmp$sim[[i]]),2))) else NULL)

all_plots <- list()

# Test the different variants of plots based on the file _inputs/tests_scatter_plot.csv
invisible(lapply(1:nrow(tmp), function(i) {
  test_that(paste0("Test #",tmp$Number[[i]]), {

    if (tmp$version[i]) {
      test_plot <- plot(tmp$sim[[i]], tmp$sim2[[i]], obs = obs, type = "scatter",
                        select_scat = tmp$select_scat[[i]],
                        all_situations = tmp$all_situations[i],
                        shape_sit = tmp$shape_sit[i],
                        situation_group = tmp$situation_group[[i]])
    } else {
      test_plot <- plot(tmp$sim[[i]], obs = obs, type = "scatter",
                        select_scat = tmp$select_scat[[i]],
                        all_situations = tmp$all_situations[i],
                        shape_sit = tmp$shape_sit[i],
                        situation_group = tmp$situation_group[[i]])
    }

    if (any(is.na(test_plot))) {

      message(paste("Scatter Plot #",tmp$Number[[i]],
                    " decribed in file _inputs/tests_scatter_plot.csv not yet implemented (plot return NA)"))

    } else {

      expect_true(is.list(test_plot))
      expect_equal(length(test_plot), tmp$length[[i]])
      expect_equal(names(test_plot), tmp$name[[i]])
      init_col <- if (tmp$init_col[i]=="NULL") NULL else tmp$init_col[i]
      init_shape <- if (tmp$init_shape[i]=="NULL") NULL else tmp$init_shape[i]
      init_linetype <- if (tmp$init_linetype[i]=="NULL") NULL else tmp$init_linetype[i]
      init_group <- if (tmp$init_group[i]=="NULL") NULL

      ## Check the number and type of layers are correct
      layers_class <- sapply(test_plot[[1]]$layers, function(x) class(x$geom)[1])
      expect_equal(length(grep("GeomPoint",layers_class)), tmp$nb_geom_point[i])
      expect_equal(length(grep("GeomSmooth",layers_class)), tmp$nb_geom_smooth[i])
      expect_equal(length(grep("GeomAbline",layers_class)), tmp$nb_abline[i])

      ## Check attributes in plot labels
      expect_equal(test_plot[[1]]$labels$col, init_col)
      expect_equal(test_plot[[1]]$labels$shape, init_shape)
      expect_equal(test_plot[[1]]$labels$linetype, init_linetype)
      expect_equal(test_plot[[1]]$labels$group, init_group)

      ## Check attributes in plot layers
      if (tmp$version_col[i]!="NULL") {
        version_col <- tmp$version_col[i]
        id_layers_with_colour <- sapply(test_plot[[1]]$layers, function(y) "colour" %in% attributes(y$mapping)$names)
        expect_equal(all(sapply(test_plot[[1]]$layers[id_layers_with_colour]$mapping$colour,
                                function(x) grepl(version_col, rlang::eval_tidy(x)))), TRUE)
      }
      if (tmp$version_shape[i]!="NULL") {
        version_shape <- tmp$version_shape[i]
        id_layers_with_shape <- sapply(test_plot[[1]]$layers, function(y) "shape" %in% attributes(y$mapping)$names)
        expect_equal(all(sapply(test_plot[[1]]$layers[id_layers_with_shape]$mapping$shape,
                                function(x) grepl(version_shape, rlang::eval_tidy(x)))), TRUE)
      }
      if (tmp$version_linetype[i]!="NULL") {
        version_linetype <- tmp$version_linetype[i]
        id_layers_with_linetype <- sapply(test_plot[[1]]$layers, function(y) "linetype" %in% attributes(y$mapping)$names)
        expect_equal(all(sapply(test_plot[[1]]$layers[id_layers_with_linetype]$mapping$linetype,
                                function(x) grepl(version_linetype, rlang::eval_tidy(x)))), TRUE)
      }

      ## add title for visual inspection of the graph
      test_plot <- lapply(test_plot, function(x) {
        x +
          ggplot2::labs(caption=paste0("Plot #",i,"\n",tmp$Title[[i]])) +
          ggplot2::theme(plot.caption = ggplot2::element_text(hjust=0.5, color="red"))
      })

      lapply(names(test_plot), function(y) {
        make_snapshot(
          paste0(prefix,"_fig.",i,"_",tmp$Title[[i]],"_",y, pkg_version),
          test_plot[[y]],
          tmpdir
        )
      }
      )

      all_plots <<- c(all_plots, test_plot)

    }


  })
}))


# Generate a pdf including all the variants of plots for visual inspection
if (!testthat:::on_ci()) {
  save_plot_pdf(all_plots,out_dir = tmpdir,file_name = "all_plots_scatter")
  print(paste("Plots saved in pdf format in ",tmpdir))
}

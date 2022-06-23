#' Plot situations by group of simulation
#'
#' @description simulation outputs for one or several situations with or without
#'  observations, eventually grouped by a model version (or any group actually)
#'
#' @param ...  Simulation outputs (each element= model version), each being a
#'  named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param obs_sd  A list (each element= situation) of standard deviations of
#' observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable
#'  in Y) or scatter (simulated VS observed)
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param var Vector of variable names for which plots have to be created.
#' Optional, all variables considered by default.
#' @param title A vector of plot titles, named by situation. Use the situation
#' name if `NULL`, recycled if length one.
#' @param all_situations Boolean (default = TRUE). If `TRUE`, plot all
#' situations on the same graph.
#' @param overlap A list of lists containing the variables to represent on the
#'  same graph
#' when `type = "dynamic"`.
#' @param successive A list of lists containing the situations to be represented
#'  as a contiguous sequence. Dates of variables must be continuous within a
#'  sequence of situations.
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param shape_sit Shape to differentiate between situations when
#' `all_situations= TRUE`. See details.
#' @param situation_group A list of lists of situations to gather when
#' `shape_sit= "group"`.
#' @param reference_var Variable selected on x-axis when `type= "scatter"` and
#'  `select_scat= "res"`. It is possible to select
#' between observation and simulation of the reference variable. (examples :
#'  reference_var = "lai_n_obs", reference_var = "mafruit_sim")
#' @param force Continue if the plot is not possible ? E.g. no observations for
#'  scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and o
#' bservations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with observations, and simulations outputs when there
#'  are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals (observations-simulations)
#' in Y.
#'
#' @details The `shape_sit` argument can be:
#' * "none" (the default): Same shape for all situations.
#' * "txt": Writes the name of the situation above each point.
#' * "symbol": One shape for each situation.
#' * "group": One shape for each group of situations described in
#' `situation_group`.
#'
#' @note The error bar will be equal to 2*`obs_sd` on each side of the point to
#' have 95% confidence.
#'
#' @note The plots titles are given by their situation name.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for a
#' situation
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Importing an example with three situations with observation:
#' workspace <- system.file(file.path("extdata", "stics_example_1"),
#'   package = "CroPlotR"
#' )
#' situations <- SticsRFiles::get_usms_list(
#'   usm_path =
#'     file.path(workspace, "usms.xml")
#' )
#' sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)
#' obs <- SticsRFiles::get_obs(workspace = workspace, usm = situations)
#'
#' plot(sim, obs = obs)
#' }
plot.cropr_simulation <- function(..., obs = NULL, obs_sd = NULL,
                                  type = c("dynamic", "scatter"),
                                  select_dyn = c("sim", "common", "obs", "all"),
                                  select_scat = c("sim", "res"), var = NULL,
                                  title = NULL, all_situations = TRUE,
                                  overlap = NULL,successive = NULL,
                                shape_sit = c("none", "txt", "symbol", "group"),
                                  situation_group = NULL, reference_var = NULL,
                                  force = TRUE, verbose = TRUE) {
  plot_situations(...,
    obs = obs, obs_sd = obs_sd, type = type, select_dyn = select_dyn,
    select_scat = select_scat, var = var, title = title,
    all_situations = all_situations, overlap = overlap,
    successive = successive, shape_sit = shape_sit,
    situation_group = situation_group, reference_var = reference_var,
    force = force, verbose = verbose, formater = format_cropr
  )
}

#' @rdname plot.cropr_simulation
autoplot.cropr_simulation <- function(
  ..., obs = NULL, obs_sd = NULL,
  type = c("dynamic", "scatter"),
  select_dyn = c("sim", "common", "obs", "all"),
  select_scat = c("sim", "res"), var = NULL,
  title = NULL, all_situations = TRUE,
  overlap = NULL, successive = NULL,
  shape_sit = c("none", "txt", "symbol", "group"),
  situation_group = NULL, reference_var = NULL,
  force = TRUE, verbose = TRUE) {
  plot_situations(...,
              obs = obs, obs_sd = obs_sd, type = type, select_dyn = select_dyn,
    select_scat = select_scat, var = var, title = title,
    all_situations = all_situations, overlap = overlap,
    successive = successive, shape_sit = shape_sit,
    situation_group = situation_group, reference_var = reference_var,
    force = force, verbose = verbose, formater = format_cropr
  )
}

#' Generate a plot based on soil characteristics
#'
#' Generates a plot of type \code{type} reflecting soil charactersitics of
#' the soil data object \code{soil}, possibly complemented by information from other
#' data objects (eg. weather, situation).
#'
#'
#' @param soil A soil data object.
#' @param type The type of plot to be generated. Possibilities include
#' * `"thickness.mswc"` -- Soil thickness agains soil maximum water content, colour shows soil organic nitrogen concentration..
#' @param supp_args A list of varaibles:threshold_Tmax et threshold_Tmin
#' @param situation A situation data object.
#' @param histogram Draw graph in histogram-like form?
#' @param interactive Transform output to an interactive `plotly` plot?
#' @param verbose Print details on the console while executing the function?
#' @param ... Arguments to pass on to the specific plot function.
#' @return The plot of type \code{type} with data from on the soil data object \code{soil}.
#' @details Use the \code{\link{set_soil}}, \code{{set_weather}}
#' functions to create respective data objects from user-given data.
#' See vignette 'List of supplied plot types' for all possible plot types.
#' @export
#' @examples
#' # load example data
#' workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
#' soil_data <- readRDS(file.path(workspace, "soil_data_wide.rds"))
#' soil <- set_soil(
#'     soil_data,
#'     id = "name",
#'     layer_thickness = list("epc", "cm"),
#'     layer_water_field_cap = list("HCCF", "g/g"),
#'     layer_water_wilting_pt = list("HMINF", "g/g"),
#'     layer_bulk_density = list("DAF", "g/cm^3"),
#'     organic_N_conc = list("norg", "g/g")
#' )
#'
#' # create plot
#' p <- plot_soil(soil, type = "thickness.mswc")
#'
plot_soil <- function(soil, type="all",supp_args=NULL,  situation=NULL, histogram=NULL, interactive = FALSE, verbose = FALSE, ...){#interactive = NULL
  possible_types <- c(
    "thickness.mswc",
    "type2"
  )
  # check if given type argument is admissible
  type <- match.arg(type, c("all", possible_types))
  # use generic plot function to create plot
  plot_generic_input(type, soil, weather=NULL, supp_args=NULL, situation, histogram, interactive, verbose, ...)
  # ToDo: implement type argument all
}

#' Generate a plot based on weather characteristics
#'
#' Generates a plot of type \code{type} reflecting weather characteristics of
#' the weather data object \code{weather}, possibly complemented by information from other
#' data objects (eg. situation).
#'
#'
#' @param weather A weather data object.
#' @param type The type of plot to be generated. Possibilities include
#' * `"limiting.temperatures"` -- Number of hot days against number of cold days, colour and form show station site and year.
#' @param threshold_Tmin The threshold temperature that defines cold days, =0 by default
#' @param threshold_Tmax The threshold temperature that defines hot days. =35 by default
#' @param situation A situation data object.
#' @param histogram Draw graph in histogram-like form?
#' @param interactive Transform output to an interactive `plotly` plot?
#' @param verbose Print details on the console while executing the function?
#' @param ... Arguments to pass on to the specific plot function.
#' @return The plot of type \code{type} with data from on the weather data object \code{weather}.
#' @details Use the \code{\link{set_weather}}, \code{{set_weather}}.
#' functions to create respective data objects from user-given data.
#' See vignette 'List of supplied plot types' for all possible plot types.
#' @export
#' @examples
#' # load example data
#' workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
#' weather_data <- readRDS(file.path(workspace, "weather_data_list.rds"))
#'
#' weather <- set_weather(
#'     weather_data,
#'     id = "station",
#'     temp_day_max = list("ttmax", "celsius"),
#'     temp_day_min = list("ttmin", "celsius"),
#'     year = "year"
#' )
#'
#' # create plot
#' p <- plot_weather(weather, type = "limiting.temperatures")
plot_weather <- function(weather, type="all", threshold_Tmin=0, threshold_Tmax=35, situation=NULL, histogram=NULL, interactive = NULL, verbose = FALSE, ...){
  possible_types <- c(
    "limiting.temperatures"
  )
  # check if given type argument is admissible
  type <- match.arg(type, c("all", possible_types))
  # use generic plot function to create plot
  plot_generic_input(type, soil=NULL, weather, c(threshold_Tmin=threshold_Tmin, threshold_Tmax=threshold_Tmax), situation, histogram, interactive, verbose, ...)
  # ToDo: implement type argument all
}

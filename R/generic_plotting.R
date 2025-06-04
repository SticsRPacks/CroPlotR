#' Generic plotting function for all models
#'
#' @description Plots simulation outputs for one or several situations with or
#' without observations, eventually grouped by a model version
#' (or any group actually).
#'
#' @param ...  Simulation outputs (each element= model version), each being a
#' named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param obs_sd  A list (each element= situation) of standard deviations of
#' observations `data.frame`s (named by situation)
#' @param type The type of plot requested, either "dynamic" (date in X, variable
#'  in Y) or scatter (simulated VS observed)
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param var A vector of variables that should be displayed on the graph.
#' @param title A vector of plot titles, named by situation. Use the situation
#' name if `NULL`, recycled if length one.
#' @param all_situations Boolean (default = TRUE). If `TRUE`, plot all
#' situations on the same graph.
#' @param overlap A list of lists containing the variables to represent on the
#' same graph when `type = "dynamic"`.
#' @param successive A list of lists containing the situations to be represented
#'  as a contiguous sequence when `type = "dynamic"`
#'  (implies that the situations are correctly ordered).
#' @param shape_sit Shape to differentiate between situations when
#' `all_situations= TRUE`. See details.
#' @param situation_group A list of lists of situations to gather when
#' `shape_sit= "group"`.
#' @param reference_var Variable selected on x-axis when type is scatter and
#' select_scat is res. It is possible to select
#' between observation and simulation of the reference variable.
#' (examples : reference_var = "lai_n_obs", reference_var = "mafruit_sim")
#' @param force Continue if the plot is not possible ? E.g. no observations for
#' scatter plots. If `TRUE`, return `NULL`, else return an error (default).
#' @param verbose Boolean. Print information during execution.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and
#' observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with observations, and simulations outputs when there
#'  are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals(observations-simulations)in Y.
#'
#' @details The `shape_sit` argument can be:
#' * "none" (the default): Same shape for all situations.
#' * "txt": Writes the name of the situation above each point.
#' * "symbol": One shape for each situation.
#' * "group": One shape for each group of situations described in
#' `situation_group`.
#'
#' @note The plots titles are given by their situation name.
#'
#' @return A (printed) list of ggplot objects, each element being a plot for
#' a situation
#'
#' @keywords internal
plot_situations <- function(..., obs = NULL, obs_sd = NULL,
                            type = c("dynamic", "scatter"),
                            select_dyn = c("sim", "common", "obs", "all"),
                            select_scat = c("sim", "res"), var = NULL,
                            title = NULL, all_situations = TRUE,
                            overlap = NULL, successive = NULL,
                            shape_sit = c("none", "txt", "symbol", "group"),
                            situation_group = NULL, reference_var = NULL,
                            force = FALSE, verbose = TRUE) {
  args_list <- parse_plot_situations_args(
    list(...), obs, obs_sd, type, select_dyn, select_scat, var, title,
    all_situations, overlap, successive, shape_sit, situation_group,
    reference_var, force, verbose
  )

  dot_args <- args_list$dot_args
  obs <- args_list$obs
  obs_sd <- args_list$obs_sd
  type <- args_list$type
  select_dyn <- args_list$select_dyn
  select_scat <- args_list$select_scat
  var <- args_list$var
  title <- args_list$title
  all_situations <- args_list$all_situations
  overlap <- args_list$overlap
  successive <- args_list$successive
  shape_sit <- args_list$shape_sit
  situation_group <- args_list$situation_group
  reference_var <- args_list$reference_var
  force <- args_list$force
  verbose <- args_list$verbose
  v_names <- args_list$v_names
  one_version <- args_list$one_version
  several_sit <- args_list$several_sit
  common_situations_models <- args_list$common_situations_models
  is_obs <- args_list$is_obs
  is_obs_sd <- args_list$is_obs_sd

  situations_outputs <- cat_with_situation(
    dot_args, obs, obs_sd, all_situations, v_names
  )

  sim <- situations_outputs$sim
  obs <- situations_outputs$obs
  obs_sd <- situations_outputs$obs_sd

  # Testing if the obs and sim have the same plants names:
  if (is_obs && !is.null(obs$Plant) && !is.null(sim$Plant)) {
    common_crops <- unique(sim$Plant) %in% unique(obs$Plant)
    if (any(!common_crops)) {
      cli::cli_alert_warning(paste0(
        "Observed and simulated crops are different. Observed: ",
        "{.value {unique(obs$Plant)}}, Simulated: {.value {unique(sim$Plant)}}"
      ))
    }
  }

  formated_situation_list <- lapply(
    common_situations_models,
    function(x) {
      df_sit <- format_cropr(
        sim[[x]],
        obs[[x]],
        obs_sd[[x]],
        type, select_dyn, select_scat,
        successive = successive,
        reference_var = reference_var,
        verbose = verbose
      ) %>%
        generic_formatting(
          var, overlap, situation_group, type, shape_sit,
          several_sit, length(dot_args)
        )

      if (
        is.null(df_sit) ||
          (
            is.null(df_sit$Observed) &&
              (
                type == "scatter" ||
                  select_dyn == "common" ||
                  select_dyn == "obs"
              )
          )
      ) {
        # No common observations and simulations when type=="scatter" or
        # select_dyn=="common" or select_dyn=="obs"
        if (verbose) {
          cli::cli_alert_warning(
            "No observations found for required variables for situation ",
            unique(df_sit$sit_name)
          )
        }
        if (force) {
          df_sit <- NULL
        } else {
          stop("No observations found for situation ", unique(df_sit$sit_name))
        }
      }

      df_sit
    }
  )

  names(formated_situation_list) <- common_situations_models

  p <- list()
  for (i in common_situations_models) {
    sim_situation <- formated_situation_list[[i]]
    item_case <- detect_item_case(
      type, detect_mixture(sim_situation), one_version, several_sit, overlap
    )

    plot_title <- if (!is.null(title)) {
      title[[i]]
    } else if (type == "scatter" && all_situations == TRUE) {
      "All situations"
    } else {
      i
    }
    p[[i]] <- switch(item_case,
      # Dynamic plots:
      "mixture_versions_overlap" =
        plot_dynamic_mixture_versions_overlap(sim_situation, i,
          title = plot_title
        ),
      "mixture_versions_no_overlap" =
        plot_dynamic_mixture_versions(sim_situation, i, title = plot_title),
      "mixture_no_versions_overlap" =
        plot_dynamic_mixture_overlap(sim_situation, i, title = plot_title),
      "mixture_no_versions_no_overlap" = plot_dynamic_mixture(sim_situation, i,
        title = plot_title
      ),
      "non_mixture_versions_overlap" =
        plot_dynamic_versions_overlap(sim_situation, i, title = plot_title),
      "non_mixture_versions_no_overlap" =
        plot_dynamic_versions(sim_situation, i, title = plot_title),
      "non_mixture_no_versions_overlap" =
        plot_dynamic_overlap(sim_situation, i, title = plot_title),
      "non_mixture_no_versions_no_overlap" =
        plot_dynamic(sim_situation, i, successive, title = plot_title),

      # Scatter plots:
      "mixture_versions_situations" = NA,
      "mixture_versions_per_situations" = NA,
      "mixture_no_versions" =
        plot_scat_mixture_allsit(
          sim_situation, i, select_scat, shape_sit,
          reference_var, is_obs_sd,
          title = plot_title
        ),
      "non_mixture_versions_situations" = NA,
      "non_mixture_versions_per_situations" = NA,
      "non_mixture_no_versions_situations" =
        plot_scat_allsit(
          sim_situation, i, select_scat, shape_sit,
          reference_var, is_obs_sd,
          title = plot_title
        ),
      "non_mixture_no_versions_per_situations" = NA
    )
  }

  names(p) <- common_situations_models

  return(p)
}


#' Plot statistics
#'
#' @param x The output of [summary.cropr_simulation()]
#' @param xvar The variable to use in x, either the group or the situation
#' (the other is used for colouring)
#' @param type The type of plot requested, either "bar" (bar plot) or "radar"
#' (radar chart)
#' @param group_bar Way to display the different statistical criteria when
#' `type= "bar"`. See details.
#' @param crit_radar Statistical criterion chosen to be displayed on the
#' radar chart.
#' @param title The plot title
#' @param force Continue if the plot is not possible ? E.g. no observations
#' for scatter plots. If `TRUE`, return `NULL`, else return an error (default).
#' @param verbose Boolean. Print information during execution.
#' @param ... Other arguments to pass (for backward compatibility only)
#'
#' @details The `group_bar` argument can be:
#' * "rows" (the default): One line of graphs per statistical criterion
#' * "stack": Bars of each statistical criterion stacked
#' * "dodge": Bars of each statistical criterion side by side
#'
#' @return Return a ggplot object with statistics
#'
#' @export
#'
#' @rdname plot.statistics
#'
#' @examples
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
#' # R2 and nRMSE stats for the simulation:
#' stats <- summary(sim, obs = obs, stats = c("R2", "nRMSE"))
#' plot(stats)
#'
#' # Change the group name:
#' stats <- summary("stics v9.0" = sim, obs = obs, stats = c("R2", "nRMSE"))
#' plot(stats)
#'
#' # R2 and nRMSE stats for two groups of simulations:
#' summary(sim1 = sim, sim2 = sim, obs = obs, stats = c("R2", "nRMSE"))
#'
plot.statistics <- function(x, xvar = c("group", "situation"),
                            type = c("bar", "radar"),
                            group_bar = c("rows", "stack", "dodge"),
                            crit_radar = NULL,
                            title = NULL, force = FALSE, verbose = TRUE, ...) {
  xvar <- match.arg(xvar, c("group", "situation"))
  type <- match.arg(type, c("bar", "radar"))
  group_bar <- match.arg(group_bar, c("rows", "stack", "dodge"))

  is_one_group <- length(unique(x$group)) == 1 # test if there is one group only
  is_all_situations <- unique(unique(x$situation) == "all_situations")
  # test if there are all situations

  nvar <- length(unique(x$variable))

  x <-
    x %>%
    reshape2::melt(
      id.vars = c("group", "situation", "variable"),
      variable.name = "statistic"
    )

  if (type == "bar") {
    if (is.null(title)) {
      title <- ""
    }

    if (xvar == "group") {
      filling <- quote(.data$situation)
      xvariable <- quote(.data$group)
      if (group_bar == "rows") {
        showlegend <- !is_all_situations
      } else {
        showlegend <- length(unique(x$statistic)) > 1
      }
    } else {
      filling <- quote(.data$group)
      xvariable <- quote(.data$situation)
      if (group_bar == "rows") {
        showlegend <- !is_one_group
      } else {
        showlegend <- length(unique(x$statistic)) > 1
      }
    }

    if (group_bar == "rows") {
      x <-
        x %>%
        ggplot2::ggplot(ggplot2::aes(y = .data$value, x = !!xvariable)) +
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data$statistic),
          cols = ggplot2::vars(.data$variable), scales = "free"
        ) +
        ggplot2::geom_col(ggplot2::aes(fill = !!filling), position = "dodge") +
        ggplot2::ggtitle(title)
    } else {
      x <-
        x %>%
        ggplot2::ggplot(ggplot2::aes(y = .data$value, x = !!xvariable)) +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!filling),
          cols = ggplot2::vars(.data$variable), scales = "free"
        ) +
        ggplot2::geom_col(ggplot2::aes(fill = .data$statistic),
          position = group_bar
        ) +
        ggplot2::ggtitle(title)
    }

    # Rotate variable names if too many variables
    if (nvar > 8) {
      x <- x + ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 90))
    }

    # Rotate situation names if they are on x-axis
    if (xvar == "situation" || (group_bar == "stack" && xvar == "situation")) {
      x <- x + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }

    if (!showlegend) {
      x <- x + ggplot2::guides(fill = "none")
    }

    # No need to label x-axis if only one value
    if ((xvar == "situation" && is_all_situations) ||
      (xvar == "group" && is_one_group)) {
      x <- x + ggplot2::xlab("") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    }

    # No need to label rows if only one
    if (group_bar != "rows" && ((xvar == "group" && is_all_situations) ||
      (xvar == "situation" && is_one_group))) {
      x <- x + ggplot2::theme(strip.text.y = ggplot2::element_blank())
    }
  } else {
    if (is.null(crit_radar)) {
      if (verbose) {
        cli::cli_alert_warning("{.code crit_radar} argument missing")
      }
      if (force) {
        return(NULL)
      } else {
        stop("No statistical criteria to plot. Use `force = TRUE` to avoid this error.")
      }
    }

    x <- x %>% dplyr::filter(.data$statistic == crit_radar)
    # v_names <- unique(x$group)

    x <-
      x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$variable, y = .data$value,
        group = .data$group, colour = .data$group,
        fill = .data$group
      )) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_polygon(linewidth = 1, alpha = 0.2) +
      ggplot2::xlab("") +
      ggplot2::ylab(paste0(crit_radar)) +
      ggplot2::ggtitle(if (is.null(title)) {
        paste0(crit_radar)
      } else {
        title
      }) +
      ggplot2::scale_x_discrete() +
      ggplot2::ggproto("CoordRadar",
        ggplot2::CoordPolar,
        theta = "x", r = "y", start = -pi / 6,
        direction = sign(1), is_linear = function(coord) TRUE
      )
  }


  x
}

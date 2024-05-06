#' Generic plotting of a situation
#'
#' @description Plots outputs of a model (and observations) for one situation.
#' This function is used as a generic plotting function for any models.
#' To use it with your own model, please provide a wrapper function around your
#' model to format the outputs used by this function (see [format_cropr()] for a
#' template), and then provide your custom function as an argument to this one.
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#' @param obs_sd  A data.frame of standard deviations of observations
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param var A vector of variables that should be displayed on the graph.
#' @param title The plot title
#' @param all_situations Boolean (default = TRUE). If `TRUE`, plot all
#'  situations on the same graph.
#' @param overlap A list of lists containing the variables to represent on
#' the same graph when `type = "dynamic"`.
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
#' scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and
#' observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' * "obs": all variables with observations, and simulations outputs
#' when there are some
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
#' @note The error bar will be equal to 2*`obs_sd` on each side of the point to
#' have 95% confidence.
#'
#' @importFrom rlang .data
#' @importFrom utils head
#' @return A ggplot object
#' @keywords internal
#'
plot_generic_situation <- function(sim, obs = NULL, obs_sd = NULL,
                                   type = c("dynamic", "scatter"),
                                   select_dyn = c("sim", "common", "obs", "all"),
                                   select_scat = c("sim", "res"), var = var,
                                   title = NULL,
                                   all_situations = TRUE, overlap = NULL,
                                   successive = NULL,
                                   shape_sit = c("none", "txt", "symbol", "group"),
                                   situation_group = NULL, total_vers = 1,
                                   num_vers = 1,
                                   reference_var = NULL, force = TRUE,
                                   verbose = TRUE) {
  is_obs <- !is.null(obs) && nrow(obs) > 0
  is_obs_sd <- !is.null(obs_sd) && nrow(obs_sd) > 0
  several_sit <- (all_situations || !is.null(successive)) &&
    shape_sit %in% c("symbol", "group")

  # Testing if the obs and sim have the same plants names:
  if (is_obs && !is.null(obs$Plant) && !is.null(sim$Plant)) {
    common_crops <- unique(sim$Plant) %in% unique(obs$Plant)

    if (any(!common_crops)) {
      cli::cli_alert_warning(paste0(
        "Observed and simulated crops are different.
                                    Obs Plant: ",
        "{.value {unique(obs$Plant)}},
                                    Sim Plant: {.value {unique(sim$Plant)}}"
      ))
    }
  }

  formated_df <- format_cropr(
    sim, obs, obs_sd, type, select_dyn, select_scat,
    successive = successive, reference_var = reference_var
  )

  # Apply some generic transformations to the data.frame:
  formated_df <- generic_formatting(
    formated_df, var, overlap, situation_group, type, shape_sit,
    several_sit, total_vers, num_vers
  )

  # In case obs is given but no common variables between obs and sim:
  if (is.null(formated_df$Observed)) {
    is_obs <- FALSE
  }

  if (is.null(formated_df) ||
    (!is_obs && (type == "scatter" || select_dyn == "common" ||
      select_dyn == "obs"))) {
    # No common observations and simulations when type=="scatter" or
    # select_dyn=="common" or select_dyn=="obs"
    if (verbose) {
      cli::cli_alert_warning("No observations found for required variables")
    }
    if (force) {
      return(NULL)
    } else {
      stop("No observations found")
    }
  }

  aesth <- aesthetics(sim, obs,
    type = type, overlap = overlap, several_sit = several_sit,
    shape_sit = shape_sit, one_version = (total_vers == 1)
  )$plot

  # Plot the simulations:
  if (type == "dynamic") {
    situation_plot <-
      formated_df %>%
      ggplot2::ggplot(ggplot2::aes(
        y = .data$Simulated,
        x = .data$Date,
        linetype = !!aesth$linetype[[1]],
        shape = !!aesth$shape[[1]],
        color = !!aesth$color[[1]],
        group = !!aesth$group[[1]]
      )) +
      # ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::labs(
        color = names(aesth$color), linetype = names(aesth$linetype),
        shape = names(aesth$shape)
      ) +
      ggplot2::scale_shape_manual(values = c(0:40)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    # Facet based on variable or group_var according to overlap
    if (is.null(overlap)) {
      situation_plot <- situation_plot +
        ggplot2::facet_wrap(. ~ .data$variable, scales = "free")
    } else {
      situation_plot <- situation_plot +
        ggplot2::facet_wrap(. ~ .data$group_var, scales = "free")
      # + ggplot2::theme(strip.text.x = ggplot2::element_blank())
    }
    # Adding the observations if any:
    # if (is_obs) {
    #   # situation_plot <- situation_plot +
    #   #   ggplot2::geom_point(
    #   #     ggplot2::aes(
    #   #       y = .data$Observed
    #   #     ), na.rm = TRUE)
    #   if (is_obs_sd) {
    #     situation_plot <- situation_plot +
    #       ggplot2::geom_errorbar(ggplot2::aes(
    #         ymin = .data$Observed - 2 * .data$Obs_SD,
    #         ymax = .data$Observed + 2 * .data$Obs_SD
    #       ), na.rm = TRUE)
    #   }
    # }
    # Add vertical lines if sim contains successive situations
    if (!is.null(successive) && "sit_name" %in% colnames(sim)) {
      successions <- head(unique(sim$succession_date), -1)
      # NB: head(x, -1) removes the last value
      situation_plot <- situation_plot +
        ggplot2::geom_vline(
          xintercept = successions,
          linetype = "dashed", color = "grey", linewidth = 1
        )
    }
  } else {
    if (select_scat == "sim") {
      situation_plot <-
        formated_df %>%
        dplyr::filter(!is.na(.data$Observed) & !is.na(.data$Simulated)) %>%
        ggplot2::ggplot(ggplot2::aes(
          y = .data$Simulated, x = .data$Observed,
          shape = !!aesth$shape[[1]],
          linetype = !!aesth$linetype[[1]],
          color = !!aesth$color[[1]] # ,
        )) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::geom_abline(
          intercept = 0, slope = 1,
          color = "grey30", linetype = 2
        ) +
        ggplot2::geom_smooth(ggplot2::aes(
          y = .data$Simulated,
          x = .data$Observed,
          group = 1
        ),
        inherit.aes = FALSE,
        method = lm, color = "blue",
        se = FALSE, linewidth = 0.6, formula = y ~ x,
        fullrange = TRUE, na.rm = TRUE
        ) +
        # Invisible points of coordinates (y,x) allowing to have both axes at
        # the same scale
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            x = .data$Simulated,
            y = .data$Observed
          ),
          alpha = 0, na.rm = TRUE
        )

      if (is_obs_sd) {
        situation_plot <- situation_plot +
          ggplot2::geom_errorbarh(ggplot2::aes(
            xmin = .data$Observed - 2 * .data$Obs_SD,
            xmax = .data$Observed + 2 * .data$Obs_SD
          ),
          na.rm = TRUE
          )
      }
    }

    if (select_scat == "res") {
      situation_plot <-
        if (is.null(reference_var)) {
          formated_df %>%
            ggplot2::ggplot(ggplot2::aes(
              y = .data$Observed - .data$Simulated,
              x = .data$Observed,
              shape = !!aesth$shape[[1]],
              linetype = !!aesth$linetype[[1]],
              color = !!aesth$color[[1]],
              group = .data$sit_name,
            ))
        } else {
          formated_df %>%
            ggplot2::ggplot(ggplot2::aes(
              y = .data$Observed - .data$Simulated,
              x = .data$Reference, shape = !!aesth$shape[[1]],
              linetype = !!aesth$linetype[[1]],
              color = !!aesth$color[[1]],
              group = .data$sit_name
            ))
        }

      situation_plot <- situation_plot +
        ggplot2::ylab("Residuals") +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::geom_abline(
          intercept = 0, slope = 0, color = "grey30",
          linetype = 2
        ) +
        ggplot2::geom_smooth(
          data = situation_plot$data,
          ggplot2::aes(
            y = !!situation_plot$mapping$y,
            x = !!situation_plot$mapping$x,
          ),
          inherit.aes = FALSE,
          method = lm, se = FALSE, linewidth = 0.6,
          formula = y ~ x, fullrange = TRUE, na.rm = TRUE
        )
      # Invisible points of coordinates (y,x) allowing to have both axes at
      # the same scale
      # if (is.null(reference_var)) {
      #   ggplot2::geom_point(mapping = ggplot2::aes(x = .data$Observed -
      # .data$Simulated, y = .data$Observed), alpha = 0, na.rm=TRUE)
      # }else{
      #   ggplot2::geom_point(mapping = ggplot2::aes(x = .data$Observed -
      # .data$Simulated, y = .data$Reference), alpha = 0, na.rm=TRUE)
      # }
    }

    situation_plot <- situation_plot +
      formated_df %>%
      ggplot2::labs(
        shape = names(aesth$shape), linetype = names(aesth$linetype),
        color = names(aesth$color)
      ) +
      ggplot2::facet_wrap(. ~ .data$variable, scales = "free") +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::ggtitle(title) +
      if (shape_sit == "txt") {
        ggrepel::geom_text_repel(ggplot2::aes(label = .data$sit_name),
          na.rm = TRUE, show.legend = FALSE, max.overlaps = Inf
        )
      }

    if (all_situations) {
      situation_plot <- situation_plot +
        ggplot2::ggtitle(ggplot2::element_blank())
    }
    if (!is.null(reference_var)) {
      situation_plot <- situation_plot + ggplot2::xlab(reference_var)
    }
  }
  situation_plot
}


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
#' @param reference_var Variable selected on x-axis when type is scaétter and
#' select_scat is res. It is possible to select
#' between observation and simulation of the reference variable.
#' (examples : reference_var = "lai_n_obs", reference_var = "mafruit_sim")
#' @param force Continue if the plot is not possible ? E.g. no observations for
#' scatter plots. If `TRUE`, return `NULL`, else return an error.
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
                            force = TRUE, verbose = TRUE) {
  dot_args <- list(...)
  type <- match.arg(type, c("dynamic", "scatter"), several.ok = FALSE)
  select_dyn <- match.arg(select_dyn, c("sim", "common", "obs", "all"),
    several.ok = FALSE
  )

  # Now that we have one data.frame only, we can test if we have observations / obs_sd:
  is_obs <- !is.null(obs) && all(sapply(obs, function(x) nrow(x) > 0))
  is_obs_sd <- !is.null(obs_sd) && all(sapply(obs, function(x) nrow(x) > 0))

  # Early error on observations (no observations given at all but we need them):
  # NB: `generic_formatting` will check if there are common observations and simulations
  if (!is_obs && (type == "scatter" || select_dyn %in% c("common", "obs"))) {
    if (verbose) {
      cli::cli_alert_warning(
        c(
          "Observations are required but not provided, ",
          "did you provide `obs = ...`?"
        )
      )
    }
    if (force) {
      return(NULL)
    } else {
      stop(
        "Observations are required but not provided, ",
        "did you provide `obs = ...`?"
      )
    }
  }

  select_scat <- match.arg(select_scat, c("sim", "res"), several.ok = FALSE)
  shape_sit <- match.arg(shape_sit, c("none", "txt", "symbol", "group"),
    several.ok = FALSE
  )

  if (select_scat == "res" || shape_sit != "none") {
    type <- "scatter"
  }

  # Disable all_situations when type=="dynamic" temporarily
  if (type == "dynamic") {
    all_situations <- FALSE
  }

  # Enable successive and overlap only when type=="dynamic"
  if (!is.null(successive) || !is.null(overlap)) {
    type <- "dynamic"
  }

  # Text on scatter plots only if all_situations
  if (shape_sit != "none") {
    all_situations <- TRUE
  }

  # Enable reference variable edit only when plotting residual scatter plot
  if (!is.null(reference_var)) {
    type <- "scatter"
    select_scat <- "res"
  }

  if (!is.null(situation_group)) {
    shape_sit <- "group"
  }

  if (shape_sit == "group" && is.null(situation_group)) {
    if (verbose) {
      cli::cli_alert_warning("Argument `situation_group` must be defined
                             when `shape_sit` is 'group'")
    }
    if (force) {
      return(NULL)
    } else {
      stop("Argument `situation_group` not defined")
    }
  }

  several_sit <- (all_situations || !is.null(successive)) &&
    shape_sit %in% c("symbol", "group")

  # Name the models:
  v_names <- names(dot_args)
  if (is.null(v_names) || length(v_names) < length(dot_args)) {
    v_names <- paste0("Version_", seq_along(dot_args))
    names(dot_args) <- v_names
  }

  # Don't show group in legend if only one:
  if (length(v_names) == 1) {
    one_version <- TRUE
  } else {
    one_version <- FALSE
  }

  # If there are successive situations:
  if (!is.null(successive)) {
    # Cat situations that need to be represented as a contiguous sequence(dynamic)
    list_rot <- cat_successive(dot_args, obs, successive)
    dot_args <- list_rot[[1]]
    obs <- list_rot[[2]]
  }

  # Intersect situations between versions
  common_situations_models <- names(dot_args[[1]])

  if (length(dot_args) > 1) {
    for (index in 2:length(dot_args)) {
      common_situations_models <- intersect(
        common_situations_models,
        names(dot_args[[index]])
      )
    }
  }

  if (length(title) == 1) {
    if (!all_situations) {
      title <- rep(title, length(common_situations_models))
      names(title) <- common_situations_models
    } else {
      title <- NULL
    }
  }

  if (!is.null(title) && length(title) != length(common_situations_models) &&
    is.null(names(title))) {
    if (verbose) {
      cli::cli_alert_danger("Situations number is different from model(s)
                            outputs, please name the {.code title} argument
                            with the situations names.")
    }
    # Situations number is different from models outputs, can't guess
    # which title is for which situation.
    stop("title argument is not a named list")
  }

  if (!is.null(title) && is.null(names(title))) {
    # title is provided by the user, is not named, but has same length than
    # common_situations_models, so we guess it:
    names(title) <- common_situations_models
  }

  # Format the data into a list of situations, with the situation repeated
  # as a column name, and the version also as a column name. In the case
  # of `all_situations==TRUE`, the situations are concatenated together
  # into one situation called "all_situations" (the data is a list of
  # one situation). The true situation name is still kept in the column
  # `sit_name` though.
  if (all_situations) {
    # If all_situations, cat all situations together for each version:
    list_data <- cat_situations(dot_args, obs, obs_sd)
    sim <- unlist(list_data[[1]], recursive = FALSE)
    names(sim) <- v_names
    sim <- list(all_situations = bind_rows(sim, .id = "version"))
    obs <- list_data[[2]]
    obs_sd <- list_data[[3]]
    common_situations_models <- "all_situations"
  } else {
    # If not all_situations, add a column to each data.frame to identify the
    # situation:
    list_data <- add_situation_col(dot_args, obs, obs_sd)
    # And bind the version data.frames together:
    sim <- cat_versions(list_data[[1]])
    obs <- list_data[[2]]
    obs_sd <- list_data[[3]]
  }

  # Testing if the obs and sim have the same plants names:
  if (is_obs && !is.null(obs$Plant) && !is.null(sim$Plant)) {
    common_crops <- unique(sim$Plant) %in% unique(obs$Plant)
    if (any(!common_crops)) {
      cli::cli_alert_warning(paste0(
        "Observed and simulated crops are different.
                                    Obs Plant: ",
        "{.value {unique(obs$Plant)}},
                                    Sim Plant: {.value {unique(sim$Plant)}}"
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

    p[[i]] <- switch(item_case,
      # Dynamic plots:
      "mixture_versions_overlap" = NA,
      "mixture_versions_no_overlap" = NA,
      "mixture_no_versions_overlap" = NA,
      "mixture_no_versions_no_overlap" = plot_dynamic_mixture(sim_situation, i),
      "non_mixture_versions_overlap" = NA,
      "non_mixture_versions_no_overlap" = plot_dynamic_versions(sim_situation,i),
      "non_mixture_no_versions_overlap" = NA,
      "non_mixture_no_versions_no_overlap" = NA,

      # Scatter plots:
      "mixture_versions_situations" = NA,
      "mixture_versions_per_situations" = NA,
      "mixture_no_versions" =
        plot_scat_mixture_allsit(
          sim_situation, i, select_scat, shape_sit,
          reference_var, is_obs_sd,
          title = if (i=="all_situations") NULL else i
        ),
      "non_mixture_versions_situations" = NA,
      "non_mixture_versions_per_situations" = NA,
      "non_mixture_no_versions_situations" = NA,
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
#' for scatter plots. If `TRUE`, return `NULL`, else return an error.
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
                            title = NULL, force = TRUE, verbose = TRUE, ...) {
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
        stop("No statistical criteria to plot")
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

#' Format simulations and observations from CropR format to a format usable by
#' CroplotR
#'
#' @description Format simulations (and observations if any) for plotting. This
#' function can be used as a template to include other models in CroPlotR.
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame
#' @param obs_sd A data.frame with observation standard deviations
#' @param type The type of plot required, either "dynamic" or "scatter"
#' @param select_dyn Which data to plot when `type= "dynamic"`? See details.
#' @param select_scat Which data to plot when `type= "scatter"`? See details.
#' @param successive A list of lists containing the situations to be represented
#'  as a contiguous sequence when type = "dynamic" (dates should be contiguous)
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param reference_var Variable selected on x-axis when type is scatter and
#' select_scat is res. It is possible to select between observation and
#' simulation of the reference variable.
#' @param verbose Logical value for displaying information while running.
#'
#' @details The `select_dyn` argument can be:
#' * "sim" (the default): all variables with simulations outputs, and
#' observations when there are some
#' * "common": variables with simulations outputs and observations in common
#' (used when `type= "scatter"` )
#' * "obs": all variables with observations, and simulations outputs when there
#'  are some
#' * "all": all variables with any observations or simulations outputs
#'
#' @details The `select_scat` argument can be:
#' * "sim" (the default): plots observations in X and simulations in Y.
#' * "res": plots observations in X and residuals(observations-simulations)in Y.
#'
#' @importFrom rlang .data
#' @importFrom dplyr "%>%"
#'
#' @return A pre-formatted `data.frame` or `NULL` if the formatting is not
#' possible (e.g. type="scatter" but no common variables in obs and sim).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # remotes::install_github("SticsRPacks/SticsRPacks")
#' workspace <- system.file(file.path("extdata", "stics_example_1"),
#'   package = "CroPlotR"
#' )
#' situation <- SticsRFiles::get_usms_list(
#'   file =
#'     file.path(workspace, "usms.xml")
#' )[1]
#' sim <- SticsRFiles::get_sim(workspace = workspace, usm = situation)
#' obs <- SticsRFiles::get_obs(workspace = workspace, usm = situation)
#' formated_df <- format_cropr(
#'   sim$`IC_Wheat_Pea_2005-2006_N0`,
#'   obs$`IC_Wheat_Pea_2005-2006_N0`
#' )
#' options(max.print = 100)
#' formated_df
#' }
format_cropr <- function(sim, obs = NULL, obs_sd = NULL,
                         type = c("dynamic", "scatter"),
                         select_dyn = c("sim", "common", "obs", "all"),
                         select_scat = c("sim", "res"),
                         successive = NULL, reference_var = NULL,
                         verbose = TRUE) {
  type <- match.arg(type, c("dynamic", "scatter"), several.ok = FALSE)
  select_dyn <- match.arg(select_dyn, c("sim", "common", "obs", "all"),
    several.ok = FALSE
  )
  select_scat <- match.arg(select_scat, c("sim", "res"), several.ok = FALSE)

  is_obs <- !is.null(obs) && isTRUE(nrow(obs) > 0)
  is_obs_sd <- !is.null(obs_sd) && isTRUE(nrow(obs_sd) > 0)

  is_mixture_sim <- detect_mixture(sim)
  is_mixture_obs <- detect_mixture(obs)

  if (is_mixture_obs && is_obs && is.null(obs$Plant)) {
    stop("Detected intercrop from simulation, but the 'Plant'
         column is missing from the observations.")
  }

  # Treating Dominance as a factor if any (for plotting reasons):
  if (is_mixture_sim) {
    sim$Dominance <- factor(
      sim$Dominance,
      levels = c("Principal", "Associated")
    )
  }

  # Adding Dominance to obs if any:
  if (is_obs && is_mixture_obs) {
    if (is.null(obs$Dominance)) {
      # Add Dominance to obs:
      corresp_table <-
        sim %>%
        dplyr::group_by(.data$Dominance) %>%
        dplyr::summarise(Plant = unique(.data$Plant))
      obs <- dplyr::full_join(obs, corresp_table, by = "Plant")
      if (is_obs_sd) {
        obs_sd <- dplyr::full_join(obs_sd, corresp_table, by = "Plant")
      }
    } else {
      obs$Dominance <- factor(obs$Dominance,
        levels = c("Principal", "Associated")
      )
      if (is_obs_sd) {
        obs_sd$Dominance <- factor(obs_sd$Dominance,
          levels = c("Principal", "Associated")
        )
      }
    }
  }

  if (!is_obs && select_dyn %in% c("obs", "common")) {
    stop(
      paste(
        "No observations found, impossible to select",
        "`select_dyn = 'obs'` or `select_dyn = 'common'`"
      )
    )
  }

  # Take all simulated variables as lowercase:
  s_lower <- unlist(lapply(colnames(sim), tolower))

  # Take all observed variables as lowercase (if any):
  if (is_obs) {
    o_lower <- unlist(lapply(colnames(obs), tolower))
    inter <- intersect(s_lower, o_lower)

    # Check if there are duplicated variable names in the observations,
    # and take the values that are not NA to replace the NA values of the
    # first column:
    if (length(o_lower) != length(unique(o_lower))) {
      double <- o_lower[which(duplicated(o_lower))]
      if (verbose) {
        cli::cli_alert_warning(paste0("Two columns have the same name with
                                      different typographies of the variable
                                      name : ", double))
      }
      for (d in double) {
        to_replace <- colnames(obs)[which(o_lower == d)]
        obs[which(is.na(obs[, to_replace[1]])), to_replace[1], drop = TRUE] <-
          obs[which(is.na(obs[, to_replace[1]])), to_replace[2], drop = TRUE]
        if (is_obs_sd) {
          obs_sd[which(is.na(obs_sd[, to_replace[1]])),
            to_replace[1],
            drop = TRUE
          ] <-
            obs_sd[
              which(is.na(obs_sd[, to_replace[1]])), to_replace[2],
              drop = TRUE
            ]
        }
      }
    }
  } else {
    inter <- s_lower
  }

  # Get common situations
  inter_sit <- intersect(unique(obs$sit_name), unique(sim$sit_name))

  # Plot all simulations, and only obs that are simulated
  if (is_obs && (type == "scatter" || select_dyn %in% c("sim", "common"))) {
    ind <- colnames(obs)[which(o_lower %in% inter)]
    obs <- obs[, ind]
    if (is_obs_sd) {
      obs_sd <- obs_sd[, ind]
    }
    # protection for call to summary (no sit_name):
    if ("sit_name" %in% names(obs)) {
      ind <- obs$sit_name %in% inter_sit
      obs <- obs[ind, ]
      if (is_obs_sd) {
        ind <- obs_sd$sit_name %in% inter_sit
        obs_sd <- obs_sd[ind, ]
      }
    }
  }

  # Plot all observations, and only sim that are observed
  if (is_obs && select_dyn %in% c("obs", "common") || type == "scatter") {
    ind <- colnames(sim)[which(s_lower %in% inter)]
    sim <- sim[, ind]
    obs <- obs[, unique(colnames(obs))]
    # protection for call to summary (no sit_name):
    if ("sit_name" %in% names(sim)) {
      ind <- sim$sit_name %in% inter_sit
      sim <- sim[ind, ]
    }
  }

  # Check if there are common variables in sim/obs but with different casing:
  if (is_obs) {
    o_lower <- lapply(colnames(obs), tolower)
    # If so, replace the variables in obs with the ones in sim:
    for (col in colnames(sim)) {
      if (tolower(col) %in% o_lower && !(col %in% colnames(obs))) {
        colnames(obs)[which(o_lower == tolower(col))] <-
          colnames(sim)[which(colnames(sim) == col)]
      }
    }
  }

  if (is_mixture_sim && is_mixture_obs) {
    rem_vars <- NULL
    melt_vars <- c("Date", "Plant", "Dominance")
  } else {
    rem_vars <- c("Plant")
    melt_vars <- "Date"
  }
  if (!is.null(successive)) {
    rem_vars <- c(rem_vars, "succession_date")
  }

  if ("sit_name" %in% colnames(sim)) {
    melt_vars <- c(melt_vars, "sit_name")
  }

  # By default, we use the same variables to melt and then to join:
  join_vars <- c(melt_vars, "variable")

  # But if there are several versions of the model, we add the version
  # for melting, but not for joining because the obs and obs_sd are the
  # same for all versions:
  if ("version" %in% colnames(sim)) {
    melt_vars_sim <- c(melt_vars, "version")
  } else {
    melt_vars_sim <- melt_vars
  }

  # Identify which columns are character vectors:
  string_cols <- names(sim)[sapply(sim, is.character)]

  # Add them to the variables removed from the data frame,
  # but remove the ones that are used for melting:
  rem_vars <- setdiff(union(rem_vars, string_cols), melt_vars_sim)

  # Create data frame like sim or obs to change reference variable when
  # residual scatter plot
  if (!is.null(reference_var)) {
    ref_var <- substr(reference_var, 1, stringr::str_length(reference_var) - 4)
    ref_type <- substr(
      reference_var, stringr::str_length(reference_var) - 2,
      stringr::str_length(reference_var)
    )
    if (ref_type == "obs") {
      ref <- obs
    } else if (ref_type == "sim") {
      ref <- sim
    } else if (ref_type == "res") {
      ref <- semi_join(sim, obs, by = melt_vars)
      tmp_obs <- semi_join(obs, sim, by = melt_vars)
      ref[, ref_var] <- dplyr::arrange(tmp_obs, ref$sit_name) %>%
        dplyr::select(tidyselect::all_of(ref_var)) - ref[, ref_var]
    } else {
      stop(
        "The variable name given in the `reference_var` argument ",
        "should end with `_sim`, `_obs` or `_res`, found: ",
        ref_type
      )
    }

    # Return an error if the reference variable is not in the data:
    if (!(ref_var %in% colnames(sim))) {
      stop(
        "The variable name (", ref_var,
        ") given in the `reference_var` argument (",
        reference_var,
        ") is not in the simulation data frame. Available variables are: ",
        paste(setdiff(colnames(sim), c(melt_vars, rem_vars)), collapse = ", ")
      )
    }

    # Make a dataframe with only the variables, and overwrite the values with
    # the reference variable:
    ref_tmp <- dplyr::select(ref, -tidyselect::any_of(c(melt_vars, rem_vars)))
    for (col in colnames(ref_tmp)) {
      ref_tmp[, col] <- ref[, ref_var]
    }
    ref[, colnames(ref_tmp)] <- ref_tmp
    ref <-
      ref %>%
      dplyr::select(-tidyselect::any_of(rem_vars)) %>%
      reshape2::melt(
        id.vars = melt_vars, na.rm = TRUE,
        value.name = "Reference"
      )

    ref$variable <- as.character(ref$variable) # to avoid factors
  }

  # Making the data:
  df <-
    sim %>%
    #dplyr::select(-tidyselect::any_of(rem_vars)) %>%
    reshape2::melt(
      id.vars = c(melt_vars_sim,rem_vars),
      na.rm = TRUE,
      value.name = "Simulated"
    )

  if (is_obs) {
    obs <-
      obs %>%
      dplyr::select(-tidyselect::any_of(rem_vars)) %>%
      reshape2::melt(id.vars = melt_vars, na.rm = TRUE, value.name = "Observed")

    if (is_obs_sd) {
      obs_sd <-
        obs_sd %>%
        dplyr::select(-tidyselect::any_of(rem_vars)) %>%
        reshape2::melt(id.vars = melt_vars, na.rm = TRUE, value.name = "Obs_SD")
    }

    if (select_dyn == "obs" || select_dyn == "common" || type == "scatter") {
      if (is.null(obs$variable)) {
        # No observations for the required variables here.
        return(NULL)
      }
    } else {
      if (is.null(obs$variable)) {
        # No observations for the required variables here.
        return(df)
      }
    }
    obs$variable <- as.character(obs$variable) # to avoid factors
    if (is_obs_sd) {
      obs_sd$variable <- as.character(obs_sd$variable) # to avoid factors
    }

    if (is.null(df$variable)) {
      # No common variables between obs and sim (case where select_dyn=="common"
      # or type=="scatter")
      return(obs)
    } else {
      df$variable <- as.character(df$variable)
    }

    df <- dplyr::full_join(df, obs, by = join_vars)

    # Add standard deviation to data frame
    if (is_obs_sd) {
      df <- dplyr::full_join(df, obs_sd, by = join_vars)
    }

    # Add reference variable to data frame (when type is residual scatter)
    if (!is.null(reference_var)) {
      df <- dplyr::full_join(df, ref, by = join_vars)
    }
  }

  # We want the residuals too if select_scat == "res"
  if (select_scat == "res") {
    df$Residuals <- df$Observed - df$Simulated
  }

  return(df)
}

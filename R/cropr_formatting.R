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
#' @param all_situations Boolean (default = FALSE). If `TRUE`, plot all
#' situations on the same graph.
#' If `TRUE`, \code{sim} and \code{obs} are respectively an element of the first
#'  element and the
#' second element of the output of cat_situations.
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
#' workspace= system.file(file.path("extdata", "stics_example_1"),
#' package = "CroPlotR")
#' situation= SticsRFiles::get_usms_list(file =
#' file.path(workspace,"usms.xml"))[1]
#' sim= SticsRFiles::get_sim(workspace = workspace, usm = situation)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm  = situation)
#' formated_df= format_cropr(sim$`IC_Wheat_Pea_2005-2006_N0`,
#' obs$`IC_Wheat_Pea_2005-2006_N0`)
#' options(max.print= 100)
#' formated_df
#' }
format_cropr <- function(sim, obs = NULL, obs_sd = NULL,
                         type = c("dynamic", "scatter"),
                         select_dyn = c("sim", "common", "obs", "all"),
                         select_scat = c("sim", "res"), all_situations = FALSE,
                         successive = NULL, reference_var = NULL,
                         verbose = TRUE) {
  type <- match.arg(type, c("dynamic", "scatter"), several.ok = FALSE)
  select_dyn <- match.arg(select_dyn, c("sim", "common", "obs", "all"),
    several.ok = FALSE
  )
  select_scat <- match.arg(select_scat, c("sim", "res"), several.ok = FALSE)

  is_obs <- !is.null(obs) && isTRUE(nrow(obs) > 0)
  is_obs_sd <- !is.null(obs_sd) && isTRUE(nrow(obs_sd) > 0)

  is_Dominance <- grep("Dominance", x = colnames(sim), fixed = TRUE)
  if (length(is_Dominance) > 0) {
    is_mixture <- length(unique(sim[[is_Dominance]])) > 1
  } else {
    is_mixture <- FALSE
  }

  if (is_mixture && is_obs && is.null(obs$Plant)) {
    stop("Detected intercrop from simulation, but the 'Plant'
         column is missing from the observations.")
  }

  # Treating Dominance as a factor if any (for plotting reasons):
  if (is_mixture && length(unique(sim$Dominance)) > 1) {
   sim$Dominance <- factor(sim$Dominance, levels = c("Principal", "Associated"))
  }

  # Adding Dominance to obs if any:
  if (is_obs && is_mixture) {
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

  # Only plotting common variables:
  if (is_obs && ((type == "dynamic" && select_dyn == "sim") ||
    (type == "dynamic" && select_dyn == "common") || type == "scatter")) {
    # Plot all simulations, and only obs that are simulated
    s_lower <- lapply(colnames(sim), tolower)
    o_lower <- lapply(colnames(obs), tolower)
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
      obs_sd[which(is.na(obs_sd[, to_replace[1]])), to_replace[2], drop = TRUE]
        }
      }
    }
    inter <- intersect(s_lower, o_lower)
    ind <- colnames(obs)[which(o_lower %in% inter)]
    obs <- obs[, ind]
    if (is_obs_sd) {
      obs_sd <- obs_sd[, ind]
    }
  }

  if (select_dyn == "obs" || select_dyn == "common" || type == "scatter") {
    if (is_obs) {
      # Plot all observations, and only sim that are observed
      ind <- colnames(sim)[which(s_lower %in% inter)]
      sim <- sim[, ind]
      diff <- setdiff(colnames(obs), colnames(sim))
      for (d in diff) {
        colnames(obs)[which(tolower(colnames(obs)) == tolower(d))] <-
          colnames(sim)[which(tolower(colnames(sim)) == tolower(d))]
      }
      obs <- obs[, unique(colnames(obs))]
    } else {
      return(NULL)
    }
  }

  # Check if there are common variables with different lettering
  if (is_obs) {
    o_lower <- lapply(colnames(obs), tolower)
    for (col in colnames(sim)) {
      if (tolower(col) %in% o_lower && !col %in% colnames(obs)) {
        colnames(sim)[which(colnames(sim) == col)] <-
          colnames(obs)[which(o_lower == tolower(col))]
      }
    }
  }

  if (is_mixture && length(unique(sim$Dominance)) > 1) {
    rem_vars <- NULL
    melt_vars <- c("Date", "Plant", "Dominance")
  } else {
    rem_vars <- c("Plant")
    melt_vars <- "Date"
  }

  if ("Sit_Name" %in% colnames(sim)) {
    melt_vars <- c(melt_vars, "Sit_Name")
  }

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
    } else {
      ref <- sim
    }
    ref_tmp <- dplyr::select(ref, -tidyselect::any_of(c(melt_vars, rem_vars)))
    for (col in colnames(ref_tmp)) {
      ref_tmp[, col] <- ref[, ref_var]
    }
    ref[, colnames(ref_tmp)] <- ref_tmp

    ref <-
      ref %>%
      dplyr::select(-tidyselect::any_of(rem_vars)) %>%
      reshape2::melt(id.vars = melt_vars, na.rm = TRUE,
                     value.name = "Reference")

    ref$variable <- as.character(ref$variable) # to avoid factors
  }

  # Making the data:
  df <-
    sim %>%
    dplyr::select(-tidyselect::any_of(rem_vars)) %>%
    reshape2::melt(id.vars = melt_vars, na.rm = TRUE, value.name = "Simulated")

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

    df <- dplyr::full_join(df, obs, by = c(melt_vars, "variable"))

    # Add standard deviation to data frame
    if (is_obs_sd) {
      df <- dplyr::full_join(df, obs_sd, by = c(melt_vars, "variable"))
    }

    # Add reference variable to data frame (when type is residual scatter)
    if (!is.null(reference_var)) {
      df <- dplyr::full_join(df, ref, by = c(melt_vars, "variable"))
    }
  }

  return(df)
}

#' Format simulation data and observation data in order to consider all
#' situations
#'
#' @description Format simulation data and observation data in a list of a
#' single data frame named "all_situations"
#'
#' @param list_sim A list (each element= version) of a list
#' (each element= situation) of simulations `data.frame`s
#' @param obs A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param obs_sd A list (each element= situation) of `data.frame`s for the
#' standard deviation of the observations (named by situation)
#' @param force Continue if the plot is not possible ? E.g. no observations for
#' scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A list of three : a list (each element=version) of a list of a single
#' simulations `data.frame` named "all_situations", a list of a single
#' observations `data.frame` named "all_situations", and the same for obs_sd.
#'
#' @keywords internal
cat_situations <-
  function(list_sim = NULL, obs = NULL, obs_sd = NULL, force = TRUE,
           verbose = TRUE) {
    sits <- lapply(list_sim, names)
    V_names <- names(list_sim)

    list_sim <-
      lapply(seq_along(list_sim), function(x) {
        allsim <- bind_rows(list_sim[[x]], .id = "Sit_Name")

        # Add dominance and plant in sim data of sole crops if one of the other
        # situations is a mixture
        if ("Dominance" %in% colnames(allsim)) {
          for (sit_name in sits[[x]]) {
            if (length(unique(obs[[sit_name]]$Plant)) == 1) {
              allsim$Plant[allsim$Sit_Name == sit_name] <-
                unique(obs[[sit_name]]$Plant)
            }
          }
        }

        allsim <- list(allsim)
        names(allsim) <- "all_situations"
        attr(allsim, "class") <- "cropr_simulation"

        allsim
      })

    names(list_sim) <- V_names

    # situations <- names(obs)
    if (!is.null(obs)) {
      # bind the obs into a single dataframe
      obs <- list(bind_rows(obs, .id = "Sit_Name"))
      names(obs) <- "all_situations"
    }

    if (!is.null(obs_sd)) {
      obs_sd <- list(bind_rows(obs_sd, .id = "Sit_Name"))
      names(obs_sd) <- "all_situations"
    }

    return(list(list_sim, obs, obs_sd))
  }


#' Format simulation data and observation data in order to represent some
#' situations as a contiguous sequence
#'
#' @description Format simulation data and observation data in a list of data
#' frame(s), each corresponding to one
#' situation or several contiguous situations over time
#'
#' @param list_sim A list (each element= version) of a list (each element=
#' situation) of simulations `data.frame`s
#' @param obs A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param successive A list of lists containing the situations to be represented
#'  as a contiguous sequence
#' when `type = "dynamic"` (implies that the situations are correctly ordered).
#' @param force Continue if the plot is not possible ? E.g. no observations for
#' scatter plots. If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return A list of two : a list (each element= version) of a list of
#' simulations `data.frame` and a list of observations `data.frame`
#'
#' @keywords internal
#'
cat_successive <-
  function(list_sim, obs, successive = NULL, force = TRUE, verbose = TRUE) {
    if (is.null(obs) && is.null(list_sim)) {
      # No simulations or observations to format
      if (verbose) {
        cli::cli_alert_warning("No simulations or observations found")
      }
      if (force) {
        return(NULL)
      } else {
        stop("No simulations or observations found")
      }
    }

    if (is.null(successive)) {
      # No situations to put together
      if (verbose) {
        cli::cli_alert_warning("No situations to join found")
      }
    }

    if (!is.null(obs)) {
      for (list_succ in successive) {
        new_name <- ""
        col_obs <- c()
        new_obs <- data.frame()
        for (sit in list_succ) {
          if (length(intersect(names(obs), list_succ)) > 0) {
            new_name <- paste0(new_name, sit, " | ")
            if (sit %in% names(obs)) {
              new_obs <- dplyr::bind_rows(new_obs, obs[[sit]])
              col_obs <- c(col_obs, rep(sit, nrow(obs[[sit]])))
              obs[[sit]] <- NULL
            }
          }
        }
        if (new_name != "") {
          obs[[new_name]] <-
            dplyr::bind_cols(new_obs, data.frame("Sit_Name" = col_obs))
        }
      }
    }

    list_sim <-
      lapply(list_sim, function(sim) {
        for (list_succ in successive) {
          new_name <- ""
          col_sim <- c()
          new_sim <- data.frame()

          for (sit in list_succ) {
            if (!(sit %in% names(sim))) {
              if (verbose) {
                cli::cli_alert_warning(
                  paste(
                    "Situations in `successive` not found in simulation data:",
                    sit
                  )
                )
              }
              if (force) {
                return(NULL)
              } else {
                stop("Please enter valid situations in `succesive` parameter")
              }
            }

            # Define borders to plot a vertical line between situations
            sim[[sit]]$succession_date <- max(sim[[sit]]$Date)

            new_name <- paste0(new_name, sit, " | ")
            new_sim <- dplyr::bind_rows(new_sim, sim[[sit]])
            col_sim <- c(col_sim, rep(sit, nrow(sim[[sit]])))
            sim[[sit]] <- NULL
          }
          sim[[new_name]] <-
            dplyr::bind_cols(new_sim, data.frame("Sit_Name" = col_sim))
        }
        sim
      })

    return(list(list_sim, obs))
  }


#' Add situation as column
#'
#' Adds the situation as a column of the `data.frame`, i.e. the name of the list
#' element also becomes a column of the element.
#'
#' @param dot_args A list (each element= version) of a list
#' (each element= situation) of simulations `data.frame`s
#' @param obs A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param obs_sd A list (each element= situation) of `data.frame`s for the
#' standard deviation of the observations (named by situation)
#'
#' @return A list of three elements: the updated dot_args, obs and obs_sd.
#' @export
#'
#' @keywords internal
add_situation_col <- function(dot_args, obs, obs_sd = NULL) {
    for (i in seq_along(dot_args)) {
    sit_names <- names(dot_args[[i]])
    for (j in sit_names) {
        dot_args[[i]][[j]]$Sit_Name <- j
    }
    }

    sit_names <- names(obs)
    for (j in sit_names) {
        obs[[j]]$Sit_Name <- j
    }

    if (!is.null(obs_sd)) {
        sit_names <- names(obs_sd)
        for (j in sit_names) {
            obs_sd[[j]]$Sit_Name <- j
        }
    }

    return(list(dot_args, obs, obs_sd))
}

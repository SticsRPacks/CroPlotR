#' Parse plot situations arguments
#'
#' This function parses the arguments for `plot_situations`.
#'
#' @inheritParams plot_situations
#'
#' @return A list of parsed arguments, eventually corrected according to the
#' compatibility between options.
#'
#' @keywords internal
parse_plot_situations_args <- function(
  dot_args, obs = NULL, obs_sd = NULL,
  type = c("dynamic", "scatter"),
  select_dyn = c("sim", "common", "obs", "all"),
  select_scat = c("sim", "res"), var = NULL,
  title = NULL, all_situations = TRUE,
  overlap = NULL, successive = NULL,
  shape_sit = c("none", "txt", "symbol", "group"),
  situation_group = NULL, reference_var = NULL,
  force = FALSE, verbose = TRUE
) {
  type <- match.arg(type, c("dynamic", "scatter"), several.ok = FALSE)
  select_dyn <- match.arg(select_dyn, c("sim", "common", "obs", "all"),
    several.ok = FALSE
  )

  # Now that we have one data.frame only, we can test if we have
  # observations / obs_sd:
  is_obs <- !is.null(obs) && all(sapply(obs, function(x) nrow(x) > 0))
  is_obs_sd <- !is.null(obs_sd) && all(sapply(obs, function(x) nrow(x) > 0))

  # Early error on observations (no observations given at all but we need them):
  # NB: `generic_formatting` will check if there are common observations
  # and simulations
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
      stop(
        "Argument `situation_group` not defined.",
        " Use `force = TRUE` to avoid this error."
      )
    }
  }

  # has_distinct_situations expresses whether, in the plot being built,
  # situations are visually distinguished (e.g. by color/shape).
  # It is independent from the input 'all_situations' (which controls whether
  # we draw one plot per situation or merge in a single plot).
  # Examples:
  # - Multiple situations but all blended with the same aesthetics -> FALSE
  # - Multiple situations with distinct aesthetics (symbol/group) -> TRUE
  # - Only one situation -> FALSE
  has_distinct_situations <- (all_situations || !is.null(successive)) &&
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
    # Cat situations that need to be represented as a
    # contiguous sequence(dynamic)
    list_rot <- cat_successive(dot_args, obs, successive)
    dot_args <- list_rot[[1]]
    obs <- list_rot[[2]]
  }

  if (all_situations) {
    common_situations_models <- "all_situations"
  } else {
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
  }

  if (length(title) == 1) {
    if (!all_situations) {
      title <- rep(title, length(common_situations_models))
      names(title) <- common_situations_models
    } else {
      title <- NULL
    }
  }

  if (
    !is.null(title) &&
      length(title) != length(common_situations_models) &&
      is.null(names(title))
  ) {
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

  return(
    list(
      dot_args = dot_args,
      obs = obs,
      obs_sd = obs_sd,
      type = type,
      select_dyn = select_dyn,
      select_scat = select_scat,
      var = var,
      title = title,
      all_situations = all_situations,
      overlap = overlap,
      successive = successive,
      shape_sit = shape_sit,
      situation_group = situation_group,
      reference_var = reference_var,
      force = force,
      verbose = verbose,
      dot_args = dot_args,
      v_names = v_names,
      one_version = one_version,
      has_distinct_situations = has_distinct_situations,
      common_situations_models = common_situations_models,
      is_obs = is_obs,
      is_obs_sd = is_obs_sd
    )
  )
}

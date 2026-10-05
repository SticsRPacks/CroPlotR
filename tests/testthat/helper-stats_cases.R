# Cases used to check that the statistics stay the same when the way they are
# computed changes. The reference outputs are made by
# `_inputs/make_stats_reference.R` and stored in `_inputs/stats_reference.rds`.

#' Build the simulations / observations used for the statistics reference
#'
#' @param inputs Path to `sim_obs.RData`
#'
#' @return A named list of cases, each being a list of arguments for
#' `statistics_situations()`
stats_cases <- function(inputs = test_path("_inputs", "sim_obs.RData")) {
  env <- new.env()
  load(inputs, envir = env)
  sim <- env$sim
  obs <- env$obs
  sim_rot <- env$sim_rot

  # Second version of the intercrop simulations, slightly different:
  sim_v2 <- sim
  for (s in names(sim_v2)) {
    sim_v2[[s]]$lai_n <- sim_v2[[s]]$lai_n * 1.1
    sim_v2[[s]]$masec_n <- sim_v2[[s]]$masec_n * 0.9 + 0.05
  }

  # Synthetic observations for the rotation simulations, with edge cases:
  set.seed(42)
  obs_rot <- lapply(names(sim_rot), function(s) {
    sim_s <- sim_rot[[s]]
    dates <- sort(sample(sim_s$Date, 12))
    o <- sim_s[sim_s$Date %in% dates, c("Date", "lai_n", "masec_n", "HR_1")]
    n <- nrow(o)
    noise <- function(x) x * stats::runif(n, 0.7, 1.3) + stats::rnorm(n, 0, 0.01)
    o$lai_n <- noise(o$lai_n)
    o$lai_n[o$lai_n < 0.05] <- 0 # observed zeros (MAPE, RME)
    o$masec_n <- noise(o$masec_n)
    o$HR_1 <- noise(o$HR_1)
    o$HR_1[c(2, 5)] <- NA # missing observations
    o$Qles <- NA_real_ # variable observed only once
    o$Qles[3] <- sim_s$Qles[sim_s$Date == o$Date[3]] + 1
    o$resmes <- NA_real_ # constant observations
    o$resmes[1:4] <- 100
    o$ET <- noise(sim_s$et[sim_s$Date %in% dates]) # different casing
    o$not_simulated <- 1 # variable not in the simulations
    o$Plant <- unique(sim_s$Plant)
    o
  })
  names(obs_rot) <- names(sim_rot)

  sim_rot_v2 <- sim_rot
  for (s in names(sim_rot_v2)) {
    sim_rot_v2[[s]]$masec_n <- sim_rot_v2[[s]]$masec_n * 1.2
  }

  sim_sole <- sim[c("SC_Pea_2005-2006_N0", "SC_Wheat_2005-2006_N0")]
  attr(sim_sole, "class") <- "cropr_simulation"

  obs_missing <- obs
  obs_missing[["SC_Wheat_2005-2006_N0"]] <- NULL

  case <- function(dots, obs, ...) c(dots, list(obs = obs, ...))

  list(
    mixture_all = case(list(sim), obs),
    mixture_sit = case(list(sim), obs, all_situations = FALSE),
    mixture_versions_all = case(list(v1 = sim, v2 = sim_v2), obs),
    mixture_versions_sit = case(
      list(v1 = sim, v2 = sim_v2), obs,
      all_situations = FALSE
    ),
    mixture_missing_obs_all = case(list(sim), obs_missing),
    mixture_missing_obs_sit = case(
      list(sim), obs_missing,
      all_situations = FALSE
    ),
    mixture_stat_subset = case(
      list(sim), obs,
      stat = c("n_obs", "RMSE", "Decision")
    ),
    sole_all = case(list(sim_sole), obs),
    rotation_all = case(list(sim_rot), obs_rot),
    rotation_sit = case(list(sim_rot), obs_rot, all_situations = FALSE),
    rotation_versions_sit = case(
      list(a = sim_rot, b = sim_rot_v2), obs_rot,
      all_situations = FALSE
    )
  )
}

#' Compute the statistics of a case with a given statistics function
run_stats_case <- function(case, fun = statistics_situations) {
  out <- NULL
  utils::capture.output(
    out <- suppressWarnings(suppressMessages(
      do.call(fun, c(case, verbose = FALSE))
    ))
  )
  out
}

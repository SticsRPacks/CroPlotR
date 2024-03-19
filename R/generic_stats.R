#' Generic statistics for all situations
#'
#' @description Compute statistics for simulations of one or several situations
#' with its observations, eventually grouped by a model version
#' (or any group actually).
#'
#' @param ...  Simulation outputs (each element= model version), each being a
#' named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s
#' (named by situation)
#' @param stat A character vector of required statistics, "all" for all, or any
#' of [predictor_assessment()] (e.g. `"n"` or `"RMSE"`, or both `c("n", "RMSE")`).
#' @param all_situations Boolean (default = TRUE). If `TRUE`, computes
#' statistics for all situations.
#' @param all_plants Boolean (default = TRUE). If `TRUE`, computes statistics
#' for all plants (when applicable).
#' @param verbose Boolean. Print information during execution.
#'
#' @seealso All the functions used to compute the statistics:
#' [predictor_assessment()].
#'
#' @return A [tibble::as_tibble()] with statistics grouped by group
#' (i.e. model version) and situation
#'
#' @keywords internal
statistics_situations <- function(..., obs = NULL, stat = "all",
                                  all_situations = TRUE,
                                  all_plants = TRUE, verbose = TRUE) {
  . <- NULL
  dot_args <- list(...)

  # Name the groups if not named:
  if (is.null(names(dot_args))) {
    names(dot_args) <- paste0("Version_", seq_along(dot_args))
  }

  # Restructure data into a list of one single element if all_situations
  if (all_situations) {
    list_data <- cat_situations(dot_args, obs)
    dot_args <- list_data[[1]]
    obs <- list_data[[2]]

  } else {
    list_data <- add_situation_col(dot_args, obs)
    dot_args <- list_data[[1]]
    obs <- list_data[[2]]
    # obs_sd <- list_data[[3]]
  }


  # Compute stats (assign directly into dot_args):
  for (versions in seq_along(dot_args)) {
    class(dot_args[[versions]]) <- NULL
    # Remove the class to avoid messing up with it afterward
    for (situation in rev(names(dot_args[[versions]]))) {
      # NB: rev() is important here because if the result is NULL,
      # the situation is popped out of the list, so we want to decrement the
      # list in case it is popped (and not increment with the wrong index)
      dot_args[[versions]][[situation]] <-
        statistics(
          sim = dot_args[[versions]][[situation]],
          obs = obs[[situation]], all_situations = all_situations,
          all_plants = all_plants, verbose = verbose
        )
    }
  }

  stats <-
    lapply(dot_args, dplyr::bind_rows, .id = "situation") %>%
    dplyr::bind_rows(.id = "group") %>%
    {
      if (length(stat) == 1 && stat == "all") {
        .
      } else {
        stat <- c("group", "situation", "variable", stat)
        dplyr::select(., !!stat)
      }
    }
  class(stats) <- c("statistics", class(stats))


  return(stats)
}

#' Generic simulated/observed statistics for one situation
#'
#' @description Compute statistics for evaluation of any model outputs against
#' observations.
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#' @param all_situations Boolean (default = FALSE). If `TRUE`, computes
#' statistics for all situations. If `TRUE`, \code{sim} and \code{obs} are
#' respectively an element of the first element and the second element of the
#' output of cat_situations.
#' @param all_plants Boolean (default = TRUE). If `TRUE`, computes statistics
#' for all plants (when applicable).
#' @param verbose Boolean. Print informations during execution.
#'
#' @note Because this function has the purpose to assess model quality, all
#'       statistics are computed on dates were observations are present only.
#'       So the simulation mean is only the mean on dates with observations,
#'       not the overall simulation mean.
#'
#' @return A data.frame with statistics for each variable and possibly each
#' grouping variable.
#'
#' @importFrom reshape2 melt
#' @importFrom parallel parLapply stopCluster
#' @importFrom dplyr ungroup group_by summarise "%>%" filter
#' @importFrom stats sd
#' @examples
#' \dontrun{
#' workspace <- system.file(file.path("extdata", "stics_example_1"),
#'   package = "CroPlotR"
#' )
#' situations <- SticsRFiles::get_usms_list(
#'   usm_path =
#'     file.path(workspace, "usms.xml")
#' )
#' sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)
#' obs <- SticsRFiles::get_obs(workspace = workspace, usm = situations)
#' statistics(
#'   sim = sim$`IC_Wheat_Pea_2005-2006_N0`,
#'   obs = obs$`IC_Wheat_Pea_2005-2006_N0`,
#' )
#' }
#'
#' @keywords internal
#'
statistics <- function(sim, obs = NULL, all_situations = FALSE,
                       all_plants = TRUE, verbose = TRUE) {
  . <- NULL # To avoid CRAN check note

  is_obs <- !is.null(obs) && nrow(obs) > 0

  if (!is_obs) {
    if (verbose) cli::cli_alert_warning("No observations found")
    return(NULL)
  }

  # Testing if the obs and sim have the same plants names:
  if (is_obs && !is.null(obs$Plant) && !is.null(sim$Plant)) {
    common_crops <- unique(sim$Plant) %in% unique(obs$Plant)

    if (any(!common_crops)) {
      cli::cli_alert_warning(
        paste0(
          "Observed and simulated crops are different. Obs Plant: ",
          "{.value {unique(obs$Plant)}},
              Sim Plant: {.value {unique(sim$Plant)}}"
        )
      )
    }
  }

  # Format the data:
  formated_df <- format_cropr(sim, obs, type = "scatter")

  # In case obs is given but no common variables between obs and sim:
  if (is.null(formated_df) || is.null(formated_df$Observed)) {
    if (verbose) {
      cli::cli_alert_warning("No observations found for required variables")
    }
    return(NULL)
  }

  x <-
    formated_df %>%
    dplyr::filter(!is.na(.data$Observed) & !is.na(.data$Simulated)) %>%
    {
      if (all_plants) {
        dplyr::group_by(., .data$variable)
      } else {
        dplyr::group_by(., .data$variable, .data$Plant)
      }
    } %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      mean_obs = mean(.data$Observed, na.rm = TRUE),
      mean_sim = mean(.data$Simulated, na.rm = TRUE),
      r_means = r_means(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      sd_obs = sd(.data$Observed, na.rm = TRUE),
      sd_sim = sd(.data$Simulated, na.rm = TRUE),
      CV_obs = (.data$sd_obs / .data$mean_obs) * 100,
      CV_sim = (.data$sd_sim / .data$mean_sim) * 100,
      R2 = R2(sim = .data$Simulated, obs = .data$Observed),
      SS_res = SS_res(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      Inter = Inter(sim = .data$Simulated, obs = .data$Observed),
      Slope = Slope(sim = .data$Simulated, obs = .data$Observed),
      RMSE = RMSE(sim = .data$Simulated, obs = .data$Observed),
      RMSEs = RMSEs(sim = .data$Simulated, obs = .data$Observed),
      RMSEu = RMSEu(sim = .data$Simulated, obs = .data$Observed),
      nRMSE = nRMSE(sim = .data$Simulated, obs = .data$Observed),
      rRMSE = rRMSE(sim = .data$Simulated, obs = .data$Observed),
      rRMSEs = rRMSEs(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      rRMSEu = rRMSEu(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      pMSEs = pMSEs(sim = .data$Simulated, obs = .data$Observed),
      pMSEu = pMSEu(sim = .data$Simulated, obs = .data$Observed),
      Bias2 = Bias2(sim = .data$Simulated, obs = .data$Observed),
      SDSD = SDSD(sim = .data$Simulated, obs = .data$Observed),
      LCS = LCS(sim = .data$Simulated, obs = .data$Observed),
      rbias2 = rbias2(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      rSDSD = rSDSD(sim = .data$Simulated, obs = .data$Observed),
      rLCS = rLCS(sim = .data$Simulated, obs = .data$Observed),
      MAE = MAE(sim = .data$Simulated, obs = .data$Observed),
      FVU = FVU(sim = .data$Simulated, obs = .data$Observed),
      MSE = MSE(sim = .data$Simulated, obs = .data$Observed),
      EF = EF(sim = .data$Simulated, obs = .data$Observed),
      Bias = Bias(sim = .data$Simulated, obs = .data$Observed),
      ABS = ABS(sim = .data$Simulated, obs = .data$Observed),
      MAPE = MAPE(sim = .data$Simulated, obs = .data$Observed),
      RME = RME(sim = .data$Simulated, obs = .data$Observed),
      tSTUD = tSTUD(sim = .data$Simulated, obs = .data$Observed),
      tLimit = tLimit(
        sim = .data$Simulated,
        obs = .data$Observed
      ),
      Decision = Decision(
        sim = .data$Simulated,
        obs = .data$Observed
      )
    )

  attr(x, "description") <-
    data.frame(
      n_obs = "Number of observations",
      mean_obs = "Mean of the observations",
      mean_sim = "Mean of the simulations",
      r_means = "Ratio between mean simulated values and mean observed values (%)",
      sd_obs = "Standard deviation of the observations",
      sd_sim = "Standard deviation of the simulation",
      CV_obs = "Coefficient of variation of the observations",
      CV_sim = "Coefficient of variation of the simulation",
      R2 = "coefficient of determination for obs~sim",
      SS_res = "Residual sum of squares",
      Inter = "Intercept of regression line",
      Slope = "Slope of regression line",
      RMSE = "Root Mean Squared Error",
      RMSEs = "Systematic Root Mean Squared Error",
      RMSEu = "Unsystematic Root Mean Squared Error",
      nRMSE = "Normalized Root Mean Squared Error, CV(RMSE)",
      rRMSE = "Relative Root Mean Squared Error",
      rRMSEs = "Relative Systematic Root Mean Squared Error",
      rRMSEu = "Relative Unsystematic Root Mean Squared Error",
      pMSEs = "Proportion of Systematic Mean Squared Error in Mean Squared Error",
      pMSEu = "Proportion of Unsystematic Mean Squared Error in Mean Squared Error",
      Bias2 = "Bias squared (1st term of Kobayashi and Salam (2000) MSE decomposition)",
      SDSD = "Difference between sd_obs and sd_sim squared (2nd term of Kobayashi and Salam (2000) MSE decomposition)",
      LCS = "Correlation between observed and simulated values (3rd term of Kobayashi and Salam (2000) MSE decomposition)",
      rbias2 = "Relative bias squared",
      rSDSD = "Relative difference between sd_obs and sd_sim squared",
      rLCS = "Relative correlation between observed and simulated values",
      MAE = "Mean Absolute Error",
      FVU = "Fraction of variance unexplained",
      MSE = "Mean squared Error",
      EF = "Model efficiency",
      Bias = "Bias",
      ABS = "Mean Absolute Bias",
      MAPE = "Mean Absolute Percentage Error",
      RME = "Relative mean error (%)",
      tSTUD = "T student test of the mean difference",
      tLimit = "T student threshold",
      Decision = "Decision of the t student test of the mean difference"
    )

  return(x)
}

#' Model quality assessment
#'
#' @description
#' Provide several metrics to assess the quality of the predictions of a model
#' (see note) against observations.
#'
#' @param obs       Observed values
#' @param sim       Simulated values
#' @param risk      Risk of the statistical test
#' @param na.rm     Boolean. Remove `NA` values if `TRUE` (default)
#' @param na.action A function which indicates what should happen when the data
#' contain NAs.
#'
#' @details The statistics for model quality can differ between sources. Here is
#'          a short description of each statistic and its equation (see html
#'          version for `LATEX`):
#' \itemize{
#'   \item `n()`: number of observations (from {`dplyr`}).
#'   \item `r_means()`: Ratio between mean simulated values and mean observed
#'              values (%),
#'      computed as : \deqn{r\_means = \frac{100*\frac{\sum_1^n(\hat{y_i})}{n}}
#'             {\frac{\sum_1^n(y_i)}{n}}}{r_means = 100*mean(sim)/mean(obs)}
#'   \item `R2()`: coefficient of determination, computed using [stats::lm()]
#'   on obs~sim.
#'   \item `SS_res()`: residual sum of squares (see notes).
#'   \item `Inter()`: Intercept of regression line, computed using [stats::lm()]
#'    on sim~obs.
#'   \item `Slope()`: Slope of regression line, computed using [stats::lm()]
#'    on sim~obs.
#'   \item `RMSE()`: Root Mean Squared Error, computed as
#'             \deqn{RMSE = \sqrt{\frac{\sum_1^n(\hat{y_i}-y_i)^2}{n}}}
#'             {RMSE = sqrt(mean((sim-obs)^2)}
#'   \item `RMSEs()`: Systematic Root Mean Squared Error, computed as
#'             \deqn{RMSEs = \sqrt{\frac{\sum_1^n(\sim{y_i}-y_i)^2}{n}}}
#'             {RMSEs = sqrt(mean((fitted.values(lm(formula=sim~obs))-obs)^2)}
#'   \item `RMSEu()`: Unsystematic Root Mean Squared Error, computed as
#'             \deqn{RMSEu = \sqrt{\frac{\sum_1^n(\sim{y_i}-\hat{y_i})^2}{n}}}
#'             {RMSEu = sqrt(mean((fitted.values(lm(formula=sim~obs))-sim)^2)}
#'   \item `NSE()`: Nash-Sutcliffe Efficiency, alias of EF, provided for user
#'   convenience.
#'   \item `nRMSE()`: Normalized Root Mean Squared Error, also denoted as
#'   CV(RMSE), and computed as:
#'              \deqn{nRMSE = \frac{RMSE}{\bar{y}}\cdot100}
#'              {nRMSE = (RMSE/mean(obs))*100}
#'   \item `rRMSE()`: Relative Root Mean Squared Error, computed as:
#'              \deqn{rRMSE = \frac{RMSE}{\bar{y}}}{rRMSE = (RMSE/mean(obs))}
#'   \item `rRMSEs()`: Relative Systematic Root Mean Squared Error, computed as
#'             \deqn{rRMSEs = \frac{RMSEs}{\bar{y}}}{rRMSEs = (RMSEs/mean(obs))}
#'   \item `rRMSEu()`: Relative Unsystematic Root Mean Squared Error,
#'   computed as
#'             \deqn{rRMSEu = \frac{RMSEu}{\bar{y}}}{rRMSEu = (RMSEu/mean(obs))}
#'   \item `pMSEs()`: Proportion of Systematic Mean Squared Error in Mean
#'   Square Error, computed as:
#'              \deqn{pMSEs = \frac{MSEs}{MSE}}{pMSEs = MSEs/MSE}
#'   \item `pMSEu()`: Proportion of Unsystematic Mean Squared Error in MEan
#'   Square Error, computed as:
#'              \deqn{pMSEu = \frac{MSEu}{MSE}}{pMSEu = MSEu^2/MSE^2}
#'   \item `Bias2()`: Bias squared (1st term of Kobayashi and Salam
#'   (2000) MSE decomposition):
#'             \deqn{Bias2 = Bias^2}
#'   \item `SDSD()`: Difference between sd_obs and sd_sim squared
#'   (2nd term of Kobayashi and Salam (2000) MSE decomposition), computed as:
#'              \deqn{SDSD = (sd\_obs-sd\_sim)^2}{SDSD = (sd\_obs-sd\_sim)^2}
#'   \item `LCS()`: Correlation between observed and simulated values
#'   (3rd term of Kobayashi and Salam (2000) MSE decomposition), computed as:
#'              \deqn{LCS = 2*sd\_obs*sd\_sim*(1-r)}
#'   \item `rbias2()`: Relative bias squared, computed as:
#'              \deqn{rbias2 = \frac{Bias^2}{\bar{y}^2}}
#'              {rbias2 = Bias^2/mean(obs)^2}
#'   \item `rSDSD()`: Relative difference between sd_obs and sd_sim squared,
#'    computed as:
#'             \deqn{rSDSD = \frac{SDSD}{\bar{y}^2}}{rSDSD = (SDSD/mean(obs)^2)}
#'   \item `rLCS()`: Relative correlation between observed and simulated values,
#'    computed as:
#'              \deqn{rLCS = \frac{LCS}{\bar{y}^2}}{rLCS = (LCS/mean(obs)^2)}
#'   \item `MAE()`: Mean Absolute Error, computed as:
#'            \deqn{MAE = \frac{\sum_1^n(\left|\hat{y_i}-y_i\right|)}{n}}
#'            {MAE = mean(abs(sim-obs))}
#'   \item `ABS()`: Mean Absolute Bias, which is an alias of `MAE()`
#'   \item `FVU()`: Fraction of variance unexplained, computed as:
#'            \deqn{FVU = \frac{SS_{res}}{SS_{tot}}}{FVU = SS_res/SS_tot}
#'   \item `MSE()`: Mean squared Error, computed as:
#'            \deqn{MSE = \frac{1}{n}\sum_{i=1}^n(Y_i-\hat{Y_i})^2}
#'            {MSE = mean((sim-obs)^2)}
#'   \item `EF()`: Model efficiency, also called Nash-Sutcliffe efficiency
#'   (NSE). This statistic is related to the FVU as
#'   \eqn{EF= 1-FVU}. It is also related to the \eqn{R^2}{R2}
#'           because they share the same equation, except SStot is applied
#'           relative to the identity function (*i.e.* 1:1 line) instead of the
#'           regression line. It is computed
#'           as: \deqn{EF = 1-\frac{SS_{res}}{SS_{tot}}}{EF = 1-SS_res/SS_tot}
#'   \item `Bias()`: Modelling bias, simply computed as:
#'             \deqn{Bias = \frac{\sum_1^n(\hat{y_i}-y_i)}{n}}
#'             {Bias = mean(sim-obs)}
#'   \item `MAPE()`: Mean Absolute Percent Error, computed as:
#'            \deqn{MAPE = \frac{\sum_1^n(\frac{\left|\hat{y_i}-y_i\right|}
#'            {y_i})}{n}}{MAPE = mean(abs(obs-sim)/obs)}
#'   \item `RME()`: Relative mean error, computed as:
#'            \deqn{RME = \frac{\sum_1^n(\frac{\hat{y_i}-y_i}{y_i})}{n}}
#'            {RME = mean((sim-obs)/obs)}
#'   \item `tSTUD()`: T student test of the mean difference, computed as:
#'            \deqn{tSTUD = \frac{Bias}{\sqrt(\frac{var(M)}{n_obs})}}
#'            {tSTUD = Bias/sqrt(var(M)/n_obs)}
#'   \item `tLimit()`: T student threshold, computed using [qt()]:
#'            \deqn{tLimit = qt(1-\frac{\alpha}{2},df=length(obs)-1)}
#'            {tLimit = qt(1-risk/2,df =length(obs)-1)}
#'   \item `Decision()`: Decision of the t student test of the mean difference
#'   (can bias be considered statistically not different from 0 at alpha level
#'   0.05, i.e. 5% probability of erroneously rejecting this hypothesis?),
#'   computed as:
#'            \deqn{Decision = abs(tSTUD ) < tLimit}
#' }
#'
#' @note \eqn{SS_{res}}{SS_res} is the residual sum of squares and
#'      \eqn{SS_{tot}}{SS_tot} the total sum of squares. They are computed as:
#'       \deqn{SS_{res} = \sum_{i=1}^n (y_i - \hat{y_i})^2}
#'       {SS_res= sum((obs-sim)^2)}
#'       \deqn{SS_{tot} = \sum_{i=1}^{n}\left(y_{i}-\bar{y}\right)^2}
#'       {SS_tot= sum((obs-mean(obs))^2}
#'       Also, it should be noted that \eqn{y_i} refers to the observed values
#'       and \eqn{\hat{y_i}} to the predicted values, \eqn{\bar{y}} to the mean
#'       value of observations and \eqn{\sim{y_i}} to
#'       values predicted by linear regression.
#'
#' @return A statistic depending on the function used.
#'
#' @name predictor_assessment
#'
#' @importFrom dplyr "%>%"
#' @importFrom stats lm sd var na.omit qt cor
#'
#' @examples
#' \dontrun{
#' sim <- rnorm(n = 5, mean = 1, sd = 1)
#' obs <- rnorm(n = 5, mean = 1, sd = 1)
#' RMSE(sim, obs)
#' }
#'
NULL


#' @export
#' @rdname predictor_assessment
r_means <- function(sim, obs, na.rm = TRUE) {
  100 * mean(sim, na.rm = na.rm) / mean(obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
R2 <- function(sim, obs, na.action = stats::na.omit) {
  . <- NULL
  if (!all(is.na(sim))) {
    stats::lm(formula = obs ~ sim, na.action = na.action) %>%
      summary(.) %>%
      .$r.squared
  } else {
    NA
  }
}

#' @export
#' @rdname predictor_assessment
SS_res <- function(sim, obs, na.rm = TRUE) {
  sum((obs - sim)^2, na.rm = na.rm) # residual sum of squares
}

#' @export
#' @rdname predictor_assessment
Inter <- function(sim, obs, na.action = stats::na.omit) {
  if (!all(is.na(sim))) {
    stats::lm(formula = sim ~ obs, na.action = na.action)$coef[1]
  } else {
    NA
  }
}

#' @export
#' @rdname predictor_assessment
Slope <- function(sim, obs, na.action = stats::na.omit) {
  if (!all(is.na(sim))) {
    stats::lm(formula = sim ~ obs, na.action = na.action)$coef[2]
  } else {
    NA
  }
}

#' @export
#' @rdname predictor_assessment
RMSE <- function(sim, obs, na.rm = TRUE) {
  sqrt(mean((sim - obs)^2, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
RMSEs <- function(sim, obs, na.rm = TRUE) {
  if (!all(is.na(sim))) {
    reg <- stats::fitted.values(lm(formula = sim ~ obs))
    sqrt(mean((reg[seq_along(sim)] - obs)^2, na.rm = na.rm))
  } else {
    NA
  }
}

#' @export
#' @rdname predictor_assessment
RMSEu <- function(sim, obs, na.rm = TRUE) {
  if (!all(is.na(sim))) {
    reg <- stats::fitted.values(lm(formula = sim ~ obs))
    sqrt(mean((reg[seq_along(sim)] - sim)^2, na.rm = na.rm))
  } else {
    NA
  }
}

#' @export
#' @rdname predictor_assessment
nRMSE <- function(sim, obs, na.rm = TRUE) {
  (RMSE(sim = sim, obs = obs, na.rm = na.rm) /
    mean(obs, na.rm = na.rm)) * 100
}

#' @export
#' @rdname predictor_assessment
rRMSE <- function(sim, obs, na.rm = TRUE) {
  (RMSE(sim = sim, obs = obs, na.rm = na.rm) /
    mean(obs, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
rRMSEs <- function(sim, obs, na.rm = TRUE) {
  (RMSEs(sim = sim, obs = obs, na.rm = na.rm) /
    mean(obs, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
rRMSEu <- function(sim, obs, na.rm = TRUE) {
  (RMSEu(sim = sim, obs = obs, na.rm = na.rm) /
    mean(obs, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
pMSEs <- function(sim, obs, na.rm = TRUE) {
  RMSEs(sim, obs, na.rm)^2 / RMSE(sim, obs, na.rm)^2
}

#' @export
#' @rdname predictor_assessment
pMSEu <- function(sim, obs, na.rm = TRUE) {
  RMSEu(sim, obs, na.rm)^2 / RMSE(sim, obs, na.rm)^2
}

#' @export
#' @rdname predictor_assessment
Bias2 <- function(sim, obs, na.rm = TRUE) {
  Bias(sim, obs, na.rm = na.rm)^2
}

#' @export
#' @rdname predictor_assessment
SDSD <- function(sim, obs, na.rm = TRUE) {
  (sd(obs, na.rm = na.rm) - sd(sim, na.rm = na.rm))^2
}

#' @export
#' @rdname predictor_assessment
LCS <- function(sim, obs, na.rm = TRUE) {
  sdobs <- sd(obs, na.rm = na.rm)
  sdsim <- sd(sim, na.rm = na.rm)
  r <- 1
  if (is.na(sdobs) || is.na(sdsim)) {
    return(NA)
  }
  if (sdobs > 0 && sdsim > 0) {
    r <- cor(x = obs, y = sim, use = "pairwise.complete.obs")
  }
  2 * sdobs * sdsim * (1 - r)
}

#' @export
#' @rdname predictor_assessment
rbias2 <- function(sim, obs, na.rm = TRUE) {
  Bias2(sim, obs, na.rm = na.rm) / ((mean(obs, na.rm = na.rm))^2)
}

#' @export
#' @rdname predictor_assessment
rSDSD <- function(sim, obs, na.rm = TRUE) {
  SDSD(sim, obs, na.rm = na.rm) / ((mean(obs, na.rm = na.rm))^2)
}

#' @export
#' @rdname predictor_assessment
rLCS <- function(sim, obs, na.rm = TRUE) {
  LCS(sim, obs, na.rm = na.rm) / ((mean(obs, na.rm = na.rm))^2)
}

#' @export
#' @rdname predictor_assessment
MAE <- function(sim, obs, na.rm = TRUE) {
  mean(abs(sim - obs), na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
ABS <- function(sim, obs, na.rm = TRUE) {
  MAE(sim, obs, na.rm)
}

#' @export
#' @rdname predictor_assessment
MSE <- function(sim, obs, na.rm = TRUE) {
  mean((sim - obs)^2, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
EF <- function(sim, obs, na.rm = TRUE) {
  # Modeling efficiency
  SStot <- sum((obs - mean(obs, na.rm = na.rm))^2, na.rm = na.rm)
  # total sum of squares
  # SSreg= sum((sim-mean(obs))^2) # explained sum of squares
  1 - SS_res(sim = sim, obs = obs, na.rm = na.rm) / SStot
}

#' @export
#' @rdname predictor_assessment
NSE <- function(sim, obs, na.rm = TRUE) {
  EF(sim, obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
Bias <- function(sim, obs, na.rm = TRUE) {
  mean(sim - obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
MAPE <- function(sim, obs, na.rm = TRUE) {

  non_zero_obs_indices <- obs != 0
  sim_filtered <- sim[non_zero_obs_indices]
  obs_filtered <- obs[non_zero_obs_indices]

  if (length(sim_filtered) > 0) {
    mape <- mean(abs(sim_filtered - obs_filtered) / obs_filtered, na.rm = na.rm)
    if (any(obs == 0)) {
      message("Attention: some observed values are zero. They are filtered for the computation of MAPE")
    }
  } else {
    cat("All observed values are zero. MAPE cannot be computed.\n")
    mape <- Inf
  }
  return(mape)
}

#' @export
#' @rdname predictor_assessment
FVU <- function(sim, obs, na.rm = TRUE) {
  var(obs - sim, na.rm = na.rm) / var(obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
RME <- function(sim, obs, na.rm = TRUE) {

  # indices were obs = 0
  non_zero_obs_indices <- obs != 0
  sim_filtered <- sim[non_zero_obs_indices]
  obs_filtered <- obs[non_zero_obs_indices]

  if (length(sim_filtered) > 0) {
    rme_value <- mean((sim_filtered - obs_filtered) / obs_filtered, na.rm = na.rm)

    if (any(obs == 0)) {
      message("Attention: some observed values are zero. They are filtered for the computation of RME")
      return(rme_value)
    }

  } else {
    cat("All observed values are zero. RME cannot be computed.\n")
    rme_value <- Inf
  }

  return(rme_value)
}

#' @export
#' @rdname predictor_assessment
tSTUD <- function(sim, obs, na.rm = TRUE) {
  M <- Bias(sim, obs, na.rm = na.rm)
  M / sqrt(var(sim - obs, na.rm = na.rm) / length(obs))
}

#' @export
#' @rdname predictor_assessment
tLimit <- function(sim, obs, risk = 0.05, na.rm = TRUE) {
  # Setting value for n_obs > 140
  if (length(obs) > 140) {
    return(1.96)
  }

  if (length(obs) > 0) {
    suppressWarnings(qt(1 - risk / 2, df = length(obs) - 1))
  } else {
    return(NA)
  }
}

#' @export
#' @rdname predictor_assessment
Decision <- function(sim, obs, risk = 0.05, na.rm = TRUE) {
  Stud <- tSTUD(sim, obs, na.rm = na.rm)
  Threshold <- tLimit(sim, obs, risk = risk, na.rm = na.rm)
  if (is.na(Stud)) {
    return("Insufficient size")
  }
  if (Stud >= 0) {
    Stud <- -Stud
  }
  if (Threshold + Stud > 0) {
    return("OK")
  } else {
    return("Rejected")
  }
}

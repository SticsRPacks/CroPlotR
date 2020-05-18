#' Generic statistics for all situations
#'
#' @description Compute statistics for simulations of one or several situations with its observations, eventually grouped
#' by a model version (or any group actually).
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here (see [statistics()] and [format_stics()] for more information).
#'
#' @return A list of statistics `data.frame`s named by situation
#'
#' @keywords internal
statistics_situations= function(...,obs=NULL,formater){
  dot_args= list(...)

  # Name the groups if not named:
  if(is.null(names(dot_args))){
    names(dot_args)= paste0("Version_", seq_along(dot_args))
  }

  # Compute stats (assign directly into dot_args):
  for(versions in seq_along(dot_args)){
    class(dot_args[[versions]])= NULL # Remove the class to avoid messing up with it afterward
    for(situation in seq_along(dot_args[[versions]])){
      dot_args[[versions]][[situation]]=
        statistics(sim = dot_args[[versions]][[situation]],
                   obs = obs[[situation]], formater = formater)
    }
  }

  dot_args
}

#' Generic simulated/observed statistics for one situation
#'
#' @description Compute statistics for evaluation of any model outputs against observations, providing a
#' formater function (see [format_stics()]).
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#'
#' @note Because this function has the purpose to assess model quality, all statistics
#'       are computed on dates were observations are present only. So the simulation mean
#'       is only the mean on dates with observations, not the overall simulation mean.
#'
#' @return A data.frame with statistics for each variable and possibly each grouping variable
#' returned by the formater.
#'
#' @importFrom reshape2 melt
#' @importFrom parallel parLapply stopCluster
#' @importFrom dplyr ungroup group_by summarise "%>%" filter
#' @importFrom stats sd
#' @examples
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#' statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
#' formater= format_stics)
#'
#' @keywords internal
#'
statistics= function(sim,obs=NULL,formater){

  is_obs= !is.null(obs) && nrow(obs>0)

  if(is_obs){
    # Use all common obs and simulations only
    obs= obs[,intersect(colnames(obs),colnames(sim))]
    sim= sim[,intersect(colnames(sim),colnames(obs))]
  }else{
    warning('Observations not found, statistics will not be computed')
    return()
  }

  # Format the data:
  formated_outputs= formater(sim, obs, plot= "common")

  x=
    formated_outputs$df%>%
    dplyr::filter(!is.na(.data$Observed))%>%
    {
      if(!is.null(formated_outputs$coloring[[1]])){
        dplyr::group_by(.,.data$variable,!!formated_outputs$coloring[[1]])
      }else{
        dplyr::group_by(.,.data$variable)
      }
    }%>%
    dplyr::summarise(n_obs= n(),
                     mean_obs= mean(Observed, na.rm = T),
                     mean_sim= mean(Simulated, na.rm = T),
                     sd_obs= sd(Observed, na.rm = T),
                     sd_sim= sd(Simulated, na.rm = T),
                     CV_obs= (sd_obs/mean_obs)*100,
                     CV_sim= (sd_sim/mean_sim)*100,
                     R2= R2(sim = Simulated, obs = Observed),
                     SS_res= SS_res(sim = Simulated, obs = Observed),
                     RMSE= RMSE(sim = Simulated, obs = Observed),
                     nRMSE= nRMSE(sim = Simulated, obs = Observed),
                     MAE= MAE(sim = Simulated, obs = Observed),
                     FVU= FVU(sim = Simulated, obs = Observed),
                     MSE= MSE(sim = Simulated, obs = Observed),
                     EF= EF(sim = Simulated, obs = Observed),
                     Bias= Bias(sim = Simulated, obs = Observed),
                     ABS= ABS(sim = Simulated, obs = Observed),
                     MAPE= MAPE(sim = Simulated, obs = Observed),
                     RME= RME(sim = Simulated, obs = Observed)
    )

  attr(x, "description")=
    data.frame(n_obs= "Number of observations",
               mean_obs= "Mean of the observations",
               mean_sim= "Mean of the simulation",
               sd_obs= "Standard deviation of the observations",
               sd_sim= "Standard deviation of the simulation",
               CV_obs= "Coefficient of variation of the observations",
               CV_sim= "Coefficient of variation of the simulation",
               R2= "coefficient of determination for obs~sim",
               SS_res= "Residual sum of squares",
               RMSE= "Root Mean Squared Error",
               nRMSE= "Normalized Root Mean Squared Error, CV(RMSE)",
               MAE= "Mean Absolute Error",
               FVU= "Fraction of variance unexplained",
               MSE= "Mean squared Error",
               EF= "Model efficiency",
               Bias= "Bias",
               ABS= "Mean Absolute Bias",
               MAPE= "Mean Absolute Percentage Error",
               RME= "Relative mean error (%)"
    )

  return(x)
}

#' Model quality assessment
#'
#' @description
#' Provide several metrics to assess the quality of the predictions of a model (see note) against
#' observations.
#'
#' @param obs       Observed values
#' @param sim       Simulated values
#' @param na.rm     Boolean. Remove `NA` values if `TRUE` (default)
#' @param na.action A function which indicates what should happen when the data contain NAs.
#'
#' @details The statistics for model quality can differ between sources. Here is a
#'          short description of each statistic and its equation (see html version
#'          for `LATEX`):
#' \itemize{
#'   \item `R2()`: coefficient of determination, computed using [stats::lm()] on obs~sim.
#'   \item `SS_res()`: residual sum of squares (see notes).
#'   \item `RMSE()`: Root Mean Squared Error, computed as
#'             \deqn{RMSE = \sqrt{\frac{\sum_1^n(\hat{y_i}-y_i)^2}{n}}}{RMSE = sqrt(mean((sim-obs)^2)}
#'   \item `NSE()`: Nash-Sutcliffe Efficiency, alias of EF, provided for user convenience.
#'   \item `nRMSE()`: Normalized Root Mean Squared Error, also denoted as CV(RMSE), and computed as:
#'              \deqn{nRMSE = \frac{RMSE}{\hat{y}}\cdot100}{nRMSE = (RMSE/mean(obs))*100}
#'   \item `MAE()`: Mean Absolute Error, computed as:
#'            \deqn{MAE = \frac{\sum_1^n(\left|\hat{y_i}-y_i\right|)}{n}}{MAE = mean(abs(sim-obs))}
#'   \item `ABS()`: Mean Absolute Bias, which is an alias of `MAE()`
#'   \item `FVU()`: Fraction of variance unexplained, computed as:
#'            \deqn{FVU = \frac{SS_{res}}{SS_{tot}}}{FVU = SS_res/SS_tot}
#'   \item `MSE()`: Mean squared Error, computed as:
#'            \deqn{MSE = \frac{1}{n}\sum_{i=1}^n(Y_i-\hat{Y_i})^2}{MSE = mean((sim-obs)^2)}
#'   \item `EF()`: Model efficiency, also called Nash-Sutcliffe efficiency (NSE). This statistic is
#'           related to the FVU as \eqn{EF= 1-FVU}. It is also related to the \eqn{R^2}{R2}
#'           because they share the same equation, except SStot is applied relative to the
#'           identity function (*i.e.* 1:1 line) instead of the regression line. It is computed
#'           as: \deqn{EF = 1-\frac{SS_{res}}{SS_{tot}}}{EF = 1-SS_res/SS_tot}
#'   \item `Bias()`: Modelling bias, simply computed as:
#'             \deqn{Bias = \frac{\sum_1^n(\hat{y_i}-y_i)}{n}}{Bias = mean(sim-obs)}
#'   \item `MAPE()`: Mean Absolute Percent Error, computed as:
#'            \deqn{MAPE = \frac{\sum_1^n(\frac{\left|\hat{y_i}-y_i\right|}{y_i})}{n}}{
#'            MAPE = mean(abs(obs-sim)/obs)}
#'   \item `RME()`: Relative mean error (\%), computed as:
#'            \deqn{RME = \frac{\sum_1^n(\frac{\hat{y_i}-y_i}{y_i})}{n}}{RME = mean((sim-obs)/obs)}
#' }
#'
#' @note \eqn{SS_{res}}{SS_res} is the residual sum of squares and \eqn{SS_{tot}}{SS_tot} the total
#'       sum of squares. They are computed as:
#'       \deqn{SS_{res} = \sum_{i=1}^n (y_i - \hat{y_i})^2}{SS_res= sum((obs-sim)^2)}
#'       \deqn{SS_{tot} = \sum_{i=1}^{n}\left(y_{i}-\bar{y}\right)^2}{SS_tot= sum((obs-mean(obs))^2}
#'       Also, it should be noted that \eqn{y_i} refers to the observed values and \eqn{\hat{y_i}} to
#'       the predicted values, and \eqn{\bar{y}} to the mean value of observations.
#'
#' @return A statistic depending on the function used.
#'
#' @seealso This function was inspired from the `evaluate()` function
#'          from the `SticsEvalR` package. This function is used by [stics_eval()]
#'
#' @name predictor_assessment
#'
#' @importFrom dplyr "%>%"
#' @importFrom stats lm sd var na.omit
#'
#' @examples
#' \dontrun{
#' library(sticRs)
#' sim= rnorm(n = 5,mean = 1,sd = 1)
#' obs= rnorm(n = 5,mean = 1,sd = 1)
#' RMSE(sim,obs)
#' }
#'
NULL


#' @export
#' @rdname predictor_assessment
R2= function(sim,obs, na.action= stats::na.omit){
  .= NULL
  stats::lm(formula = obs~sim, na.action= na.action)%>%summary(.)%>%.$adj.r.squared
}

#' @export
#' @rdname predictor_assessment
SS_res= function(sim,obs,na.rm= T){
  sum((obs-sim)^2, na.rm = na.rm) # residual sum of squares
}

#' @export
#' @rdname predictor_assessment
RMSE= function(sim,obs,na.rm= T){
  sqrt(mean((sim-obs)^2, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
nRMSE= function(sim,obs,na.rm= T){
  (RMSE(sim = sim, obs = obs, na.rm= na.rm)/
     mean(obs, na.rm = na.rm))*100
}

#' @export
#' @rdname predictor_assessment
MAE= function(sim,obs,na.rm= T){
  mean(abs(sim-obs), na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
ABS= function(sim,obs,na.rm= T){
  MAE(sim,obs,na.rm)
}

#' @export
#' @rdname predictor_assessment
MSE= function(sim,obs,na.rm= T){
  mean((sim-obs)^2,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
EF=  function(sim,obs,na.rm= T){
  # Modeling efficiency
  SStot= sum((obs-mean(obs,na.rm= na.rm))^2, na.rm = na.rm) # total sum of squares
  # SSreg= sum((sim-mean(obs))^2) # explained sum of squares
  1-SS_res(sim = sim, obs = obs, na.rm = na.rm)/SStot
}

#' @export
#' @rdname predictor_assessment
NSE= function(sim,obs,na.rm= T){
  EF(sim, obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
Bias= function(sim,obs,na.rm= T){
  mean(sim-obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
MAPE= function(sim,obs,na.rm= T){
  mean(abs(sim-obs)/obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
FVU=  function(sim,obs,na.rm= T){
  var(obs-sim,na.rm = na.rm)/var(obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
RME=  function(sim,obs,na.rm= T){
  mean((sim-obs)/obs, na.rm = na.rm)
}

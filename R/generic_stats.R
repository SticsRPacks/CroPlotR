#' Generic statistics for all situations
#'
#' @description Compute statistics for simulations of one or several situations with its observations, eventually grouped
#' by a model version (or any group actually).
#'
#' @param ...  Simulation outputs (each element= model version), each being a named list of `data.frame` for each situation.
#' See examples.
#' @param obs  A list (each element= situation) of observations `data.frame`s (named by situation)
#' @param stat A character vector of required statistics, "all" for all, or any of [predictor_assessment()].
#' @param all_situations Boolean. If `TRUE`, computes statistics for all situations.
#' @param verbose Boolean. Print information during execution.
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here (see [statistics()] and [format_stics()] for more information).
#'
#' @seealso All the functions used to compute the statistics: [predictor_assessment()].
#'
#' @return A [dplyr::tibble()] with statistics grouped by group (i.e. model version) and situation
#'
#' @keywords internal
statistics_situations= function(...,obs=NULL,stat="all",all_situations=TRUE,verbose=TRUE,formater){
  .= NULL
  dot_args= list(...)

  # Name the groups if not named:
  if(is.null(names(dot_args))){
    names(dot_args)= paste0("Version_", seq_along(dot_args))
  }

  # Restructure data in order to compute statistics from all situations
  if(all_situations){
    situations_names= names(dot_args[["Version_1"]])
    # Converts simulation data into a list of one single element if all_situations
    dot_args=
      lapply(dot_args,function(x){
        allsim= NULL
        for(sit_name in situations_names){
          if(sit_name==situations_names[1]){
            allsim= x[[sit_name]]
            next()
          }
          allsim= dplyr::bind_rows(allsim,x[[sit_name]])
        }
        allsim= list(allsim)
        names(allsim)= "all_situations"
        class(allsim)= "stics_simulation"
        allsim
      })
    # Converts observation data into a list of one single element if all_situations
    allobs=NULL
    for(sit_name in situations_names){
      if(sit_name==situations_names[1]){
        allobs=obs[[sit_name]]
        next()
      }
      allobs= dplyr::bind_rows(allobs,obs[[sit_name]])
    }
    allobs= list(allobs)
    obs= allobs
    names(obs)= "all_situations"
    class(obs)= "stics_observation"
  }

  # Compute stats (assign directly into dot_args):
  for(versions in seq_along(dot_args)){
    class(dot_args[[versions]])= NULL # Remove the class to avoid messing up with it afterward
    for(situation in rev(names(dot_args[[versions]]))){
      # NB: rev() is important here because if the result is NULL, the situation is popped out of the list,
      # so we want to decrement the list in case it is popped (and not increment with the wrong index)
      dot_args[[versions]][[situation]]=
        statistics(sim = dot_args[[versions]][[situation]],
                   obs = obs[[situation]], verbose = verbose, formater = formater)
    }
  }

  stats=
    lapply(dot_args, dplyr::bind_rows, .id="situation")%>%
    dplyr::bind_rows(.id="group")%>%
    {
      if(length(stat)==1 && stat=="all"){
        .
      }else{
        stat= c("group", "situation", "variable", stat)
        dplyr::select(.,!!stat)
      }
    }
  class(stats)= c("statistics",class(stats))

  return(stats)
}

#' Generic simulated/observed statistics for one situation
#'
#' @description Compute statistics for evaluation of any model outputs against observations, providing a
#' formater function (see [format_stics()]).
#'
#' @param sim A simulation data.frame
#' @param obs An observation data.frame (variable names must match)
#' @param verbose Boolean. Print informations during execution.
#' @param formater The function used to format the models outputs and observations in a standard way. You can design your own function
#' that format one situation and provide it here.
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
#' \dontrun{
#' workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#' statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
#' formater= format_stics)
#' }
#'
#' @keywords internal
#'
statistics= function(sim,obs=NULL,verbose=TRUE,formater){
  .= NULL # To avoid CRAN check note

  is_obs= !is.null(obs) && nrow(obs>0)

  if(is_obs){
    # Use all common obs and simulations only
    obs= obs[,intersect(colnames(obs),colnames(sim))]
    sim= sim[,intersect(colnames(sim),colnames(obs))]
  }else{
    if(verbose) cli::cli_alert_warning("No observations found")
    return(NULL)
  }

  # Format the data:
  formated_outputs= formater(sim, obs, plot= "common")

  # In case obs is given but no common variables between obs and sim:
  if(is.null(formated_outputs$df$Observed) || is.null(formated_outputs)){
    if(verbose) cli::cli_alert_warning("No observations found for required variables")
    return(NULL)
  }

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
    dplyr::summarise(n_obs= dplyr::n(),
                     mean_obs= mean(.data$Observed, na.rm = T),
                     mean_sim= mean(.data$Simulated, na.rm = T),
                     sd_obs= sd(.data$Observed, na.rm = T),
                     sd_sim= sd(.data$Simulated, na.rm = T),
                     CV_obs= (.data$sd_obs/.data$mean_obs)*100,
                     CV_sim= (.data$sd_sim/.data$mean_sim)*100,
                     R2= R2(sim = .data$Simulated, obs = .data$Observed),
                     SS_res= SS_res(sim = .data$Simulated, obs = .data$Observed),
                     RMSE= RMSE(sim = .data$Simulated, obs = .data$Observed),
                     RMSEs= RMSEs(sim = .data$Simulated, obs = .data$Observed),
                     RMSEu= RMSEu(sim = .data$Simulated, obs = .data$Observed),
                     nRMSE= nRMSE(sim = .data$Simulated, obs = .data$Observed),
                     rRMSE= rRMSE(sim = .data$Simulated, obs = .data$Observed),
                     pRMSEs= pRMSEs(sim = .data$Simulated, obs = .data$Observed),
                     pRMSEu= pRMSEu(sim = .data$Simulated, obs = .data$Observed),
                     MAE= MAE(sim = .data$Simulated, obs = .data$Observed),
                     FVU= FVU(sim = .data$Simulated, obs = .data$Observed),
                     MSE= MSE(sim = .data$Simulated, obs = .data$Observed),
                     EF= EF(sim = .data$Simulated, obs = .data$Observed),
                     Bias= Bias(sim = .data$Simulated, obs = .data$Observed),
                     ABS= ABS(sim = .data$Simulated, obs = .data$Observed),
                     MAPE= MAPE(sim = .data$Simulated, obs = .data$Observed),
                     RME= RME(sim = .data$Simulated, obs = .data$Observed)
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
               RMSEs= "Systematic Root Mean Squared Error",
               RMSEu= "Unsystematic Root Mean Squared Error",
               nRMSE= "Normalized Root Mean Squared Error, CV(RMSE)",
               rRMSE= "Relative Root Mean Squared Error",
               pRMSEs= "Proportional Systematic Root Mean Squared Error",
               pRMSEu= "Proportional Unsystematic Root Mean Squared Error",
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
#'   \item `RMSEs()`: Systematic Root Mean Squared Error, computed as
#'             \deqn{RMSEs = \sqrt{\frac{\sum_1^n(\sim{y_i}-y_i)^2}{n}}}
#'             {RMSEs = sqrt(mean((fitted.values(lm(formula=sim~obs))-obs)^2)}
#'   \item `RMSEu()`: Unsystematic Root Mean Squared Error, computed as
#'             \deqn{RMSEu = \sqrt{\frac{\sum_1^n(\sim{y_i}-\hat{y_i})^2}{n}}}
#'             {RMSEu = sqrt(mean((fitted.values(lm(formula=sim~obs))-sim)^2)}
#'   \item `NSE()`: Nash-Sutcliffe Efficiency, alias of EF, provided for user convenience.
#'   \item `nRMSE()`: Normalized Root Mean Squared Error, also denoted as CV(RMSE), and computed as:
#'              \deqn{nRMSE = \frac{RMSE}{\hat{y}}\cdot100}{nRMSE = (RMSE/mean(obs))*100}
#'   \item `rRMSE()`: Relative Root Mean Squared Error, computed as:
#'              \deqn{rRMSE = \frac{RMSE}{\hat{y}}}{rRMSE = (RMSE/mean(obs))}
#'   \item `pRMSEs()`: Proportional Systematic Root Mean Squared Error, computed as:
#'              \deqn{pRMSEs = \frac{RMSEs^2}{RMSE^2}}{pRMSEs = RMSEs^2/RMSE^2}
#'   \item `pRMSEu()`: Proportional Unsystematic Root Mean Squared Error, computed as:
#'              \deqn{pRMSEu = \frac{RMSEu^2}{RMSE^2}}{pRMSEu = RMSEu^2/RMSE^2}
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
#'       the predicted values, \eqn{\bar{y}} to the mean value of observations and \eqn{\sim{y_i}} to
#'       values predicted by linear regression.
#'
#' @return A statistic depending on the function used.
#'
#' @name predictor_assessment
#'
#' @importFrom dplyr "%>%"
#' @importFrom stats lm sd var na.omit
#'
#' @examples
#' \dontrun{
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
RMSEs= function(sim,obs,na.rm= T){
  reg=stats::fitted.values(lm(formula=sim~obs))
  sqrt(mean((reg-obs)^2, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
RMSEu= function(sim,obs,na.rm= T){
  reg=stats::fitted.values(lm(formula=sim~obs))
  sqrt(mean((reg-sim)^2, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
nRMSE= function(sim,obs,na.rm= T){
  (RMSE(sim = sim, obs = obs, na.rm= na.rm)/
     mean(obs, na.rm = na.rm))*100
}

#' @export
#' @rdname predictor_assessment
rRMSE= function(sim,obs,na.rm= T){
  (RMSE(sim = sim, obs = obs, na.rm= na.rm)/
     mean(obs, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
pRMSEs= function(sim,obs,na.rm= T){
  RMSEs(sim,obs,na.rm)^2 / RMSE(sim,obs,na.rm)^2
}

#' @export
#' @rdname predictor_assessment
pRMSEu= function(sim,obs,na.rm= T){
  RMSEu(sim,obs,na.rm)^2 / RMSE(sim,obs,na.rm)^2
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

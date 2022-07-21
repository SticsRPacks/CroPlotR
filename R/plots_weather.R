#' @rdname plot_weather
#' @keywords internal
plot__limiting.temperatures <- function(weather, histogram,symbol=c("auto","Year","Site"), interactive,threshold_Tmin, threshold_Tmax,...){
  # ensure that essential variables are present

 weather <- ensure_hardWrapper(weather, c("nb_below_threshold_Tmin", "nb_above_threshold_Tmax"),"limiting_temperatures",
                              c(threshold_Tmin=threshold_Tmin, threshold_Tmax=threshold_Tmax))

  # try to find non-essential variables
  res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
  weather <- res$object
  found <- res$found

  if(is.null(histogram))
    histogram <- if(nrow(weather$data) > NB_HIST) TRUE else FALSE

  # create and return plot
  if(!histogram){
    if (symbol[1]=="auto"){
      mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                           shape= as.factor(!!found$summary_year))
      legend_colour="Site"
      legend_shape="Year"
      # p <- create_plot(
      #   weather$data,
      #   "nb_below_threshold_Tmin",
      #   "nb_above_threshold_Tmax",
      #   add_geomArgs = list(mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
      #                                            shape= as.factor(!!found$summary_year))),
      #   xlab=paste("nb days Tmin < ",threshold_Tmin," deg C"),
      #   ylab=paste("nb days Tmax >",threshold_Tmax," deg C"),
      #   legend_colour="Site",
      #   legend_shape="Year",
      #   ...
      # )

    }else if (symbol[1]=="Site"){
      # p <- create_plot(
      #   weather$data,
      #   "nb_below_threshold_Tmin",
      #   "nb_above_threshold_Tmax",
      #   add_geomArgs = list(mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))),
      #   xlab=paste("nb days Tmin < ",threshold_Tmin," deg C"),
      #   ylab=paste("nb days Tmax >",threshold_Tmax," deg C"),
      #   legend_colour="Site",
      #   legend_shape=NULL,
      #   ...
      # )
      mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))
      legend_colour="Site"
      legend_shape=NULL

    }else if (symbol[1]=="Year"){
      # p <- create_plot(
      #   weather$data,
      #   "nb_below_threshold_Tmin",
      #   "nb_above_threshold_Tmax",
      #   add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))),
      #   xlab=paste("nb days Tmin < ",threshold_Tmin," deg C"),
      #   ylab=paste("nb days Tmax >",threshold_Tmax," deg C"),
      #   legend_colour=NULL,
      #   legend_shape="Year",
      #   ...
      # )
      mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))
      legend_colour=NULL
      legend_shape="Year"
    }else if (is.list(symbol)){
      # shape_tmp= as.factor(!!found$summary_year)
      # res=shape_tmp[is.na(pmatch(shape_tmp,symbol[1]))]
      # shape=list(list(res),symbol[1])
      # p <- create_plot(
      #   weather$data,
      #   "nb_below_threshold_Tmin",
      #   "nb_above_threshold_Tmax",
      #   add_geomArgs = list(mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
      #                                            shape= list(list(as.factor(!!found$summary_year)[is.na(pmatch(as.factor(!!found$summary_year),symbol[1]))]),symbol[1]))),
      #   xlab=paste("nb days Tmin < ",threshold_Tmin," deg C"),
      #   ylab=paste("nb days Tmax >",threshold_Tmax," deg C"),
      #   legend_colour="Site",
      #   legend_shape="Group of year",
      #   ...
      # )
   } else {stop("unexpected value for argument symbol")}
    #   colour= as.factor(!!found$summary_station_name)
    #   shape_tmp= as.factor(!!found$summary_year)
    #   res=shape[is.na(pmatch(shape_tmp,symbol[1]))]
    #   shape=list(list(res),symbol[1])
    # legend_colour="Site"
    # legend_shape="Group of year"


    p <- create_plot(
      weather$data,
      "nb_below_threshold_Tmin",
      "nb_above_threshold_Tmax",
      add_geomArgs = list(mapping=ggplot2::aes(colour= colour,
                                               shape= shape)),
      xlab=paste("nb days Tmin < ",threshold_Tmin," deg C"),
      ylab=paste("nb days Tmax >",threshold_Tmax," deg C"),
      legend_colour=legend_colour,
      legend_shape=legend_shape,
      ...
    )
    }else{
      if (is.null(interactive)||interactive==FALSE){
        p <- create_plot(
          weather$data,
          "nb_below_threshold_Tmin",
          "nb_above_threshold_Tmax",
          geom_fun = ggplot2::geom_hex,
          xlab = paste("nb days Tmin < ",threshold_Tmin," deg C"),
          ylab = paste("nb days Tmax >",threshold_Tmax," deg C"),
          legend_colour= "count situations",
          show.legend=TRUE,
          ...
        )
        }else{
        p <- create_plot(
          weather$data,
          "nb_below_threshold_Tmin",
          "nb_above_threshold_Tmax",
          geom_fun = ggplot2::geom_hex,
          xlab = paste("nb days Tmin < ",threshold_Tmin," deg C"),
          ylab = paste("nb days Tmax >",threshold_Tmax," deg C"),
          legend_colour= "count situations",
          show.legend=FALSE,
          ...
        )}

    situations <- get_hexLabels(weather$data,
                                "nb_below_threshold_Tmin",
                                "nb_above_threshold_Tmax",
                                c(found$summary_station_name, found$summary_year))
    p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
  }
  #p <- p + ggplot2::scale_shape_manual(values=seq(0,15))
  make_interactive(p, interactive, histogram)
  return(p)
}


 plot__limiting.rainfall_days <- function(weather, histogram=NULL, interactive, threshold_RainMin, threshold_RainMax,...){
   weather <- ensure_hardWrapper(weather, c("nb_below_threshold_RainMin", "nb_above_threshold_RainMax"), "limiting.rainfall_days",
                                 c(threshold_RainMin=threshold_RainMin, threshold_RainMax=threshold_RainMax))
   # try to find non-essential variables
   res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
   weather <- res$object
   found <- res$found
   # create and return plot
   if(is.null(histogram)){
     histogram <- if(nrow(weather$data)> NB_HIST) TRUE else FALSE
   }
   if(!histogram){
     p <- create_plot(
       weather$data,
       "nb_above_threshold_RainMax",
       "nb_below_threshold_RainMin",
       add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(summary_year), colour= as.factor(summary_station_name))),
       xlab=paste("nb_high_rainfall_days(Rainfall > ",threshold_RainMax,"mm)"),
       ylab=paste("nb_dry days(Rain-PET < ",threshold_RainMin," mm)"),
       legend_colour="Site",
       legend_shape="Year",
       ...
     )}
   else{
     if (is.null(interactive)||interactive==FALSE){
       p <- create_plot(
         weather$data,
         "nb_above_threshold_RainMax",
         "nb_below_threshold_RainMin",
         geom_fun = ggplot2::geom_hex,
         xlab=paste("nb_high_rainfall_days(Rainfall > ",threshold_RainMax,"mm)"),
         ylab=paste("nb_dry days(Rain-PET < ",threshold_RainMin," mm)"),
         legend_colour= "count situations",
         show.legend=TRUE,
         ...
       )
       }else{
       p <- create_plot(
         weather$data,
         "nb_above_threshold_RainMax",
         "nb_below_threshold_RainMin",
         geom_fun = ggplot2::geom_hex,
         xlab=paste("nb_high_rainfall_days(Rainfall > ",threshold_RainMax,"mm)"),
         ylab=paste("nb_dry days(Rain-PET < ",threshold_RainMin," mm)"),
         legend_colour= "count situations",
         show.legend=FALSE,
         ...
       )}
     situations <- get_hexLabels(weather$data,
                                 "nb_below_threshold_RainMin",
                                 "nb_above_threshold_RainMax",
                                 c("summary_station_name", "summary_year"))
     p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
   }
   # p <- p + ggplot2::scale_shape_manual(values=seq(0,15))
   p <- make_interactive(p, interactive, histogram)
   return(p)
 }

 plot__temperature.rainfall <- function(weather, histogram=NULL, interactive,...){
   weather <- ensure_hardWrapper(weather, c("rainfall_cumulated", "temp_mean"), "temperature_rainfall")
   res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
   weather <- res$object
   found <- res$found
   # create and return plot
   if(is.null(histogram)){
     histogram <- if(nrow(weather$data)> NB_HIST) TRUE else FALSE
   }
   if(!histogram){
     p <- create_plot(
       weather$data,
       "rainfall_cumulated",
       "temp_mean",
       add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(summary_year), colour= as.factor(summary_station_name))),
       xlab="Total rainfall",
       ylab="Annual average T mean",
       legend_colour="Site",
       legend_shape="Year",
       ...
     )}
   else{
     if (is.null(interactive)||interactive==FALSE){
       p <- create_plot(
         weather$data,
         "rainfall_cumulated",
         "temp_mean",
         geom_fun = ggplot2::geom_hex,
         xlab = "Total rainfall(mm)",
         ylab = "Annual average T mean (deg C)",
         legend_colour= "count situations",
         show.legend=TRUE,
         ...
       )
       }else{
       p <- create_plot(
         weather$data,
         "rainfall_cumulated",
         "temp_mean",
         geom_fun = ggplot2::geom_hex,
         xlab = "Total rainfall(mm)",
         ylab = "Annual average T mean (deg C)",
         legend_colour= "count situations",
         show.legend=FALSE,
         ...
       )}
     situations <- get_hexLabels(weather$data, "rainfall_cumulated", "temp_mean", c("summary_station_name", "summary_year"))
     p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
   }
   p <- make_interactive(p, interactive, histogram)
   return(p)
 }

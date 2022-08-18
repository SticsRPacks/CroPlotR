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
                          # shape= as.factor(!!weather$data$summary_year))
                             shape= as.factor(!!found$summary_year))
      legend_colour="Site"
      legend_shape="Year"

    }else if (symbol[1]=="Site"){
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))
     legend_colour="Site"
      legend_shape=NULL
    }else if (symbol[1]=="Year"){
      mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))
      legend_colour=NULL
      legend_shape="Year"
    }else if (is.list(symbol)&& !is.null(symbol)){

      # Change Summary_year column if symbol is the group defined by user
        for (grp in seq_along(symbol)) {
          years <- symbol[[grp]]
          if (!is.null(names(symbol))) {
            # weather$data$summary_year[which(weather$data$summary_year %in% years)] <-names(symbol)[[grp]]
            weather$data <- weather$data %>% mutate(summary_year=ifelse(years %in% symbol,"group of years",weather$data$summary_year))
          } else {
            found$summary_year[which(found$summary_year %in% years)] <-
            paste(years, collapse = ";")
          }
        }

      mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                           #shape= as.factor(!!weather$data$summary_year))
                           shape= as.factor(!!found$summary_year))
      legend_colour="Site"
      legend_shape="Year"
    } else {stop("unexpected value for argument symbol")
      }
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
      add_geomArgs = list(mapping=mapping),
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

  make_interactive(p, interactive, histogram)
  return(p)
}

#' @rdname plot_weather
#' @keywords internal
 plot__limiting.rainfall_days <- function(weather, histogram=NULL, symbol=c("auto","Year","Site"), interactive, threshold_RainMin, threshold_RainMax,...){
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
     if (symbol[1]=="auto"){
       mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                            shape= as.factor(!!found$summary_year))
       legend_colour="Site"
       legend_shape="Year"

     }else if (symbol[1]=="Site"){
       mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))
       legend_colour="Site"
       legend_shape=NULL
     }else if (symbol[1]=="Year"){
       mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))
       legend_colour=NULL
       legend_shape="Year"
     }else if (is.list(symbol)&& !is.null(symbol)){
       # Change Summary_year column if symbol is the group defined by user
       for (grp in seq_along(symbol)) {
         years <- symbol[[grp]]
         if (!is.null(names(symbol))) {
           weather$data$summary_year[which(weather$data$summary_year %in% years)] <-
             names(symbol)[[grp]]
         } else {
           found$summary_year[which(found$summary_year %in% years)] <-
             paste(years, collapse = ";")
         }
       }
       mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                            shape= as.factor(!!weather$data$summary_year))
       legend_colour="Site"
       legend_shape="Group of year"

     } else {stop("unexpected value for argument symbol")
     }
     p <- create_plot(
       weather$data,
       "nb_above_threshold_RainMax",
       "nb_below_threshold_RainMin",
       add_geomArgs = list(mapping=mapping),
       xlab=paste("nb_high_rainfall_days(Rainfall > ",threshold_RainMax,"mm)"),
       ylab=paste("nb_dry days(Rain-PET < ",threshold_RainMin," mm)"),
       legend_colour=legend_colour,
       legend_shape=legend_shape,
       ...
     )
   }

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
   p <- make_interactive(p, interactive, histogram)
   return(p)
 }

 #' @rdname plot_weather
 #' @keywords internal
 plot__temperature.rainfall <- function(weather, histogram=NULL, symbol=c("auto","Year","Site"), interactive,...){

   weather <- ensure_hardWrapper(weather, c("rainfall_cumulated", "temp_mean"), "temperature_rainfall")
   res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
   weather <- res$object
   found <- res$found
   # create and return plot
   if(is.null(histogram)){
     histogram <- if(nrow(weather$data)> NB_HIST) TRUE else FALSE
   }
   if(!histogram){
   if (symbol[1]=="auto"){
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                          shape= as.factor(!!found$summary_year))
     legend_colour="Site"
     legend_shape="Year"

   }else if (symbol[1]=="Site"){
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))
     legend_colour="Site"
     legend_shape=NULL
   }else if (symbol[1]=="Year"){
     mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))
     legend_colour=NULL
     legend_shape="Year"
   }else if (is.list(symbol)&& !is.null(symbol)){
     # Change Summary_year column if symbol is the group defined by user
     for (grp in seq_along(symbol)) {
       years <- symbol[[grp]]
       if (!is.null(names(symbol))) {
         weather$data$summary_year[which(weather$data$summary_year %in% years)] <-
           names(symbol)[[grp]]
       } else {
         found$summary_year[which(found$summary_year %in% years)] <-
           paste(years, collapse = ";")
       }
     }
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                          shape= as.factor(!!found$summary_year))
     legend_colour="Site"
     legend_shape="Group of year"

   } else {stop("unexpected value for argument symbol")
   }
   p <- create_plot(
     weather$data,
     "rainfall_cumulated",
     "temp_mean",
     add_geomArgs = list(mapping=mapping),
     xlab="Total rainfall",
     ylab="Annual average T mean (deg C)",
     legend_colour=legend_colour,
     legend_shape=legend_shape,
     ...
   )

 }else{
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

 #' @rdname plot_weather
 #' @keywords internal
 plot__temperature <- function(weather, histogram=NULL, ...){
   weather <- ensure_hardWrapper(weather, c("temp_day_mean"), "temperature")
   res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
   weather <- res$object
   # create and return plot
   # Tmin, Tmean and Tmax => 3 curves (Tmin, Tmean, Tmax) on the same graph
   p <- ggplot(data=weather$data_byDay, aes(x=julian))
   p <- p+geom_line(aes(y = temp_day_min, colour = "min temperature (°C)"))+
     geom_line(aes(y = temp_day_mean, colour = "mean temperature (°C)"))+
     geom_line(aes(y = temp_day_max, colour = "max temperature (°C)"))+
     facet_grid(year~station_name)+
     ggplot2::ylab("Temperature")
   return(p)
 }

 #' @rdname plot_weather
 #' @keywords internal
 plot__radiation <- function(weather, histogram=NULL,cumulate=FALSE,...){
   weather <- ensure_hardWrapper(weather, c("radiation_day"), "radiation")
   ylab="Daily global radiation"
   p <- ggplot(weather$data_byDay, aes(y=(radiation_day),x=(julian)))
   if (cumulate==TRUE){
     ylab="Cumulated global radiation"
     weather <- ensure_hardWrapper(weather, c("radiation_cumulated"), "radiation")
     p <- ggplot(weather$data_byDay, aes(y=(radiation_cumulated),x=(julian)))
   }
   # res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
   # weather <- res$object
   p <- p+geom_line()+
     facet_grid(year~station_name)+
     labs(x=NULL,y=ylab)
   return(p)
 }

 #' @rdname plot_weather
 #' @keywords internal
plot__cumulated_PET.rain <- function(weather, histogram=NULL, symbol=c("auto","Year","Site"), interactive,...){

 weather <- ensure_hardWrapper(weather, c("PET_cumulated", "rainfall_cumulated"), "cumulated_PET.rain")
 res <- ensure_softWrapper(weather, c("summary_year", "summary_station_name"))
 weather <- res$object
 found <- res$found
 # create and return plot
 if(is.null(histogram)){
   histogram <- if(nrow(weather$data)> NB_HIST) TRUE else FALSE
 }
 if(!histogram){
   if (symbol[1]=="auto"){
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                          shape= as.factor(!!found$summary_year))
     legend_colour="Site"
     legend_shape="Year"

   }else if (symbol[1]=="Site"){
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name))
     legend_colour="Site"
     legend_shape=NULL
   }else if (symbol[1]=="Year"){
     mapping=ggplot2::aes(shape= as.factor(!!found$summary_year))
     legend_colour=NULL
     legend_shape="Year"
   }else if (is.list(symbol)&& !is.null(symbol)){
     # Change Summary_year column if symbol is the group defined by user
     for (grp in seq_along(symbol)) {
       years <- symbol[[grp]]
       if (!is.null(names(symbol))) {
         found$summary_year[which(found$summary_year %in% years)] <-
           names(symbol)[[grp]]
       } else {
         found$summary_year[which(found$summary_year %in% years)] <-
           paste(years, collapse = ";")
       }
     }
     mapping=ggplot2::aes(colour=as.factor(!!found$summary_station_name),
                          shape= as.factor(!!found$summary_year))
     legend_colour="Site"
     legend_shape="Group of year"

   } else {stop("unexpected value for argument symbol")
   }
   p <- create_plot(
     weather$data,
     "rainfall_cumulated",
     "PET_cumulated",
     add_geomArgs = list(mapping=mapping),
     xlab="Cumulated rainfall",
     ylab="Cumulated PET vs. Cumulated rainfall\n",
     legend_colour=legend_colour,
     legend_shape=legend_shape,
     ...
   )

 }else{
   if (is.null(interactive)||interactive==FALSE){
     p <- create_plot(
       weather$data,
       "rainfall_cumulated",
       "PET_cumulated",
       geom_fun = ggplot2::geom_hex,
       xlab = "Cumulated rainfall",
       ylab = "Cumulated PET vs. Cumulated rainfall\n",
       legend_colour= "count situations",
       show.legend=TRUE,
       ...
     )
   }else{
     p <- create_plot(
       weather$data,
       "rainfall_cumulated",
       "PET_cumulated",
       geom_fun = ggplot2::geom_hex,
       xlab = "Cumulated rainfall",
       ylab = "Cumulated PET vs. Cumulated rainfall\n",
       legend_colour= "count situations",
       show.legend=FALSE,
       ...
     )}
   situations <- get_hexLabels(weather$data, "rainfall_cumulated", "PET_cumulated", c("summary_station_name", "summary_year"))
   p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))

 }
 p <- make_interactive(p, interactive, histogram) +geom_abline(slope = 3.6, intercept = -90, color = "red")
 return(p)
 }

 #  plot__radiation_cumulated <- function(weather, histogram=NULL,cumulate=TRUE,...){
 #   weather <- ensure_hardWrapper(weather, c("radiation_cumulated"), "radiation_cumulated")

 #   # create and return plot
 #   p<-ggplot(data=weather$data_byDay, aes(y=(radiation_cumulated),x=(julian)))
 #   p <- p+geom_line()+
 #     facet_grid(year~station_name)+
 #     labs(x=NULL,y="Cumulated global radiation\n")
 #
 #
 #   return(p)
 # }

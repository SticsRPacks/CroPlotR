#' @rdname plot__thickness.mswc
plot__limiting.temperatures <- function(weather, histogram, interactive, ...){
  # ensure that essential variables are present
  weather <- ensure_hardWrapper(weather, c("nb_below_0", "nb_above_35"), "limiting_temperatures")

  # try to find non-essential variables
  found <- NULL
  c(weather, found) %<-% ensure_softWrapper(weather, c("summary_year", "summary_station_name"))

  if(is.null(histogram))
    histogram <- if(nrow(weather$data) > NB_HIST) TRUE else FALSE

  # create and return plot
  if(!histogram){
    p <- create_plot(
      weather$data,
      "nb_below_0",
      "nb_above_35",
      add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(!!found$summary_year),
                                               colour=as.factor(!!found$summary_station_name))),
      xlab="nb days Tmin < 0°C",
      ylab="nb days Tmax > 35°C",
      legend_colour="Site",
      legend_shape="Year",
      ...
    )}
  else{
    p <- create_plot(
      weather$data,
      "nb_below_0",
      "nb_above_35",
      geom_fun = ggplot2::geom_hex,
      xlab = "nb days Tmin < 0°C",
      ylab = "nb days Tmax > 35°C",
      ...
    )
    situations <- get_hexLabels(weather$data,
                                "nb_below_0",
                                "nb_above_35",
                                c(found$summary_station_name, found$summary_year))
    p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
  }
  p <- make_interactive(p, interactive, histogram)
  return(p)
}

# plot__temperature.rainfall <- function(weather, histogram=NULL, ...){
#   weather <- ensure_wrapper(weather, c("rainfall_cumulated", "temp_mean", "summary_year", "summary_station_name"), "temperature_rainfall")
#
#   # create and return plot
#   if(is.null(histogram)){
#     histogram <- if(nrow(weather$data)>100) TRUE else FALSE
#   }
#   if(!histogram){
#     p <- plot_scatter(
#       weather$data,
#       "nb_below_0",
#       "nb_above_35",
#       add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(summary_year), colour=as.factor(summary_station_name))),
#       xlab="nb days Tmin < 0°C",
#       ylab="nb days Tmax > 35°C",
#       legend_colour="Site",
#       legend_shape="Year",
#       ...
#     )}
#   else{
#     p <- plot_scatter(
#       weather$data,
#       "nb_below_0",
#       "nb_above_35",
#       geom_fun = ggplot2::geom_hex,
#       xlab = "nb days Tmin < 0°C",
#       ylab = "nb days Tmax > 35°C",
#       ...
#     )
#     situations <- get_hexLabels(weather$data, "nb_below_0", "nb_above_35", c("summary_station_name", "summary_year"))
#     p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
#   }
#   return(p)
# }

#' Generate a graph of specified type
#'
#'
#' @param soil A `cropr_input` object containing soil data
#' @param weather A `cropr_input` object containing weather data
#' @param histogram Sould the output be in a histogram-like form?
#' @return A graph created by the `ggplot2` package
#' @details The function names of the form 'plot_*plot_type*' are required for these specific plot functions
#' to be found by the `plot_generic_input` function
#' @importFrom zeallot %<-%
plot__thickness.mswc <- function(soil, histogram, interactive, ...){
  # ensure that essential variables are present
  soil <- ensure_hardWrapper(soil, c("depth", "saturated_wtr_cap"), "thickness.mswc.norg")

  # try to find non-essential variables
  found <- NULL
  c(soil, found) %<-% ensure_softWrapper(soil, "organic_N_conc")

  if(is.null(histogram))
    histogram <- if(nrow(soil$data) > NB_HIST) TRUE else FALSE

  # create and return plot
  if(!histogram){
    p <- create_plot(
      soil$data,
      "depth",
      "saturated_wtr_cap",
      label= "id",
      xlab= "Soil thickness",
      ylab= "Soil maximum water capacity",
      legend_colour= "Organic N conc.",
      add_geomArgs=list(mapping=ggplot2::aes(colour=!!found$organic_N_conc)),
      ...
    )
  }
  else{
    p <- create_plot(
      soil$data,
      "depth",
      "saturated_wtr_cap",
      geom_fun = ggplot2::geom_hex,
      xlab= "Soil thickness",
      ylab= "Soil maximum water capacity",
      ...
    )
    situations <- get_hexLabels(soil$data,
                                "depth",
                                "saturated_wtr_cap",
                                c("id"))
    p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
  }

  p <- make_interactive(p, interactive, histogram)

  return(p)

}

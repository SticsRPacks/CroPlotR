#' Generate a graph of specified type
#'
#'
#' @param soil A `cropr_input` object containing soil data
#' @param weather A `cropr_input` object containing weather data
#' @return A graph created by the `ggplot2` package
#' @details The function names of the form 'plot_*plot_type*' are required for these specific plot functions
#' to be found by the `plot_generic_input` function
#' @importFrom zeallot %<-%
plot__thickness.mswc <- function(soil, histogram, ...){
  # ensure that essential variables are present
  soil <- ensure_hardWrapper(soil, c("depth", "saturated_wtr_cap"), "thickness.mswc.norg")

  # try to find non-essential variables
  c(soil, found) %<-% ensure_softWrapper(soil, "organic_N_conc")

  # create and return plot
  if(!histogram){
    p <- plot_scatter(
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
    p <- plot_scatter(
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


  return(p)

}

#' @rdname plot__thickness.mswc
plot__limiting.temperatures <- function(weather, histogram, ...){
  # ensure that essential variables are present
  weather <- ensure_hardWrapper(weather, c("nb_below_0", "nb_above_35"), "limiting_temperatures")

  # try to find non-essential variables
  c(weather, found) %<-% ensure_softWrapper(weather, c("summary_year", "summary_station_name"))

  # create and return plot
  if(!histogram){
    p <- plot_scatter(
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
    p <- plot_scatter(
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
  return(p)
}

plot__temperature.rainfall <- function(weather, histogram=NULL, ...){
  weather <- ensure_wrapper(weather, c("rainfall_cumulated", "temp_mean", "summary_year", "summary_station_name"), "temperature_rainfall")

  # create and return plot
  if(is.null(histogram)){
    histogram <- if(nrow(weather$data)>100) TRUE else FALSE
  }
  if(!histogram){
    p <- plot_scatter(
      weather$data,
      "nb_below_0",
      "nb_above_35",
      add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(summary_year), colour=as.factor(summary_station_name))),
      xlab="nb days Tmin < 0°C",
      ylab="nb days Tmax > 35°C",
      legend_colour="Site",
      legend_shape="Year",
      ...
    )}
  else{
    p <- plot_scatter(
      weather$data,
      "nb_below_0",
      "nb_above_35",
      geom_fun = ggplot2::geom_hex,
      xlab = "nb days Tmin < 0°C",
      ylab = "nb days Tmax > 35°C",
      ...
    )
    situations <- get_hexLabels(weather$data, "nb_below_0", "nb_above_35", c("summary_station_name", "summary_year"))
    p <- p + ggplot2::aes(label = ggplot2::after_stat(situations))
  }
  return(p)
}

get_hexLabels <- function(data, x, y, chars, trunc=8){
  if("units" %in% class(data[[x]]))
    data[[x]] <- as.numeric(data[[x]])
  if("units" %in%  class(data[[y]]))
    data[[y]] <- as.numeric(data[[y]])

  p <- ggplot2::ggplot(data) +
    ggplot2::aes(
      !!dplyr::sym(x),
      !!dplyr::sym(y)
    ) +
    ggplot2::stat_summary_hex(
      ggplot2::aes(z=(1:nrow(data)),
      label=ggplot2::after_stat(value)),
      fun = base::identity,
      geom="text"
    )

  vec <- ggplot2::ggplot_build(p)$data[[1]]$label

  selection <- data %>% dplyr::ungroup() %>% dplyr::select(all_of(as.character(chars)))

  vec %>%
    lapply(function(x) selection[x,]) %>%
    lapply(apply, 1, paste, collapse=", ") %>%
    lapply(function(x) if(length(x)<=trunc) x else c(x[1:trunc],"...")) %>%
    sapply(paste, collapse="; ")
}

#' Generate a scatter plot
#'
#' Generates a scatter plot applying different aestetics automatically.
#'
#'
#' @param x x coordiantes of the plotted data.
#' @param y y coordinates of the plotted data.
#' @param ... Additional arguments to be passed on to the ggplot's \code{geom_point} function.
#' @param title Title of the scatter plot.
#' @param label Vector of labels for the scatterd points.
#' @param max.overlaps Exclude text labels that overlap too many things. Defaults to Inf.
#' @param xlab label of the x-axis; passed on to ggplot's \code{xlab} function.
#' @param ylab label of the y-axis; passed on to ggplot's \code{xlab} function.
#' @param colour.legend Title of the colour legend.
#' @param shape.legend Title of the shape legend.
#' @param size.legend Title of the size legend.
#' @return The graph of type \code{type} with data from on the soil data object \code{soil}.
#' @details Arguments \code{colour_legend}, \code{shape_legend} and \code{size_legend} only have an effect
#' if the respective aestetic is unsed, ie. if the \code{legend} argument is used or if
#' \code{aes(color=my_color)} or \code{aes(shape=my_shape)} is passed using \code{...}.
#' @examples
#' \dontrun{
#' ToDo
#' }
#'
plot_scatter <- function(data, x, y, geom_fun=ggplot2::geom_point, title=NULL, label=NULL, xlab=NULL, ylab=NULL,
                         legend_colour=NULL, legend_shape=NULL, legend_size=NULL, add_geomArgs=NULL, ...){

  geom_args <- list(...)

  if(!is.null(add_geomArgs)){
    overwrite <- get_overwriteNames(geom_args, add_geomArgs)
    if(length(overwrite) > 0)
      warning(paste(
        "The following geometric arguments to the graph are overwritten by the user:",
        paste(overwrite, collapse = ", ")), call. = FALSE)
    geom_args <- combine.lists(geom_args, add_geomArgs)
  }
  geom_args <- symbols_applyIfClass(data, geom_args, "as.numeric", "units")

  if("units" %in% class(data[[x]])){
    xlab <- paste0(xlab, " [", as.character(units(data[[x]])), "]")
    data[[x]] <- as.numeric(data[[x]])
  }
  if("units" %in%  class(data[[y]])){
    ylab <- paste0(ylab, " [", as.character(units(data[[y]])), "]")
    data[[y]] <- as.numeric(data[[y]])
  }

  p <- ggplot2::ggplot(data) + ggplot2::aes(x=!!dplyr::sym(x), y=!!dplyr::sym(y))

  if(!is.null(legend_colour)) p <- p + ggplot2::labs(colour=legend_colour)
  if(!is.null(legend_shape)) p <- p + ggplot2::scale_shape(legend_shape)
  if(!is.null(legend_size)) p <- p + ggplot2::labs(size=legend_size)
  if(!is.null(label)) p <- p + ggplot2::aes(label=!!sym(label)) + ggrepel::geom_text_repel()
  if(!is.null(xlab)) p <- p + ggplot2::xlab(xlab)
  if(!is.null(ylab)) p <- p + ggplot2::ylab(ylab)
  if(!is.null(title)) p <- p + ggplot2::ggtitle(title)

  p <- p + do.call(geom_fun, geom_args) + ggplot2::scale_size_area()

  return(p)
}

get_namesRec <- function(list){
  names <- sapply(names(list),
                   function(name)
                     if(is.list(list[[name]])){
                       return(get_namesRec(list[[name]]))
                     }else{
                       return(name)
                     }
  )
  return(unlist(names))
}

get_overwriteNames <- function(geom_args, add_geomArgs){
  # override <- get_overrideNames(geom_args, add_geomArgs)
  # return(get_overrideMessage_rec(override))
  names1 <- geom_args %>% get_namesRec() %>% ggplot2::standardise_aes_names()
  names2 <- add_geomArgs %>% get_namesRec() %>% ggplot2::standardise_aes_names()
  return(dplyr::intersect(names1, names2))
}

combine.lists <- function(list1, list2){
  # Combine lists 'list1' and 'list2', giving precedence to elements found in 'list1':
  # that is, if $something is found in both 'list1' and 'list2',
  # the new (output) list will have the same values as 'list1' in $something
  # this function is applied recursively to all sublists

  if(is.null(list2)) return(list1)
  if(is.null(list1)) return(list2)

  list1.sublist.names <- names(list1[vapply(list1, is.list, T)])
  list2.sublist.names <- names(list2[vapply(list2, is.list, T)])
  if(any(list1.sublist.names %in% setdiff(names(list2), list2.sublist.names)) |
     any(list2.sublist.names %in% setdiff(names(list1), list1.sublist.names)))
    warning("The sublists of the two supplied lists could not be matched.")

  for(name in intersect(list1.sublist.names, list2.sublist.names)){
    list1[[name]] <- combine.lists(list1[[name]], list2[[name]])
  }

  list1.names <- names(list1)
  list2.names <- names(list2)

  new.list <- list2

  tmp <- match(list1.names, list2.names)
  w <- which(!is.na(tmp))

  if (length(w) > 0){
    # take values from list1 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[tmp] <- list1[w]

    # append elements of 'list1' with unmatched names
    new.list <- utils::modifyList(list1[-w], new.list)

  }
  else{
    new.list <- utils::modifyList(list1, new.list)
  }

  return(new.list)
}

#' Add function call to symbols/quosures in nested list whose evaluation in some data is of a given class
#'
#' @param data A data frame
#' @param geom_args A list of symbols or quosures
#' @param funName Name of the function to apply
#' @param className Name of a class
#' @return The list `geom_args` where the function `funName` is applied to all symbols whose evaluation in
#' `data` is of class `className`.
#'

symbols_applyIfClass <- function(data, geom_args, funName, className){
  isUneval <- "uneval" %in% class(geom_args)

  geom_args <- lapply(geom_args, function(x){
    if("uneval" %in% class(x))
      symbols_applyIfClass(data, x, funName, className) %>%
        structure(class = "uneval")
    else {
      xClass <-
        rlang::eval_tidy(x, data) %>%
        class()
      if(className %in% xClass)
        rlang::set_expr(x, call(funName, rlang::get_expr(x)))
      else
        x
    }
  })

  if(isUneval)
    geom_args <- structure(geom_args, class = "uneval")

  return(geom_args)
}

`geomTextRepel<-` <- function(plot, value){
  plot$layers[[gginnards::which_layers(p, "GeomTextRepel")]] <- value
  return(plot)
}

get_allPlotTypes <- function(soil){
  # get all functions of the package
  plotFunctions <- unclass(lsf.str(envir = asNamespace("CroPlotR"), all = T))
  # only keep the specific plot functions
  plotFunctions <- plotFunctions[startsWith(plotFunctions, "plot__")]
  # strip the 'plot__' prexif to get types
  plotTypes <- lapply(plotFunctions, function(x) substr(x, 7, nchar(x)))
  return(plotTypes)
}

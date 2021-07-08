#' Generate a graph: thickness, maximum  water content and organic nitrogen per soil
#'
#' Generates a graph decpicting the tickness, the maximum water content and the organis nitrogen per soil.
#'
#'
#' @param name Name of each soil.
#' @param thickness Thickness of each soil.
#' @param mswc Maximum water content of each soil.
#' @param norg Amount og organic nitrogen in each soil.
#' @param max.overlaps Exclude names of certain soils that would overlap too many things.
#' Defaults to Inf meaning that all names are shown.
#' @return The respective graph as \code{ggplot2} object.
#' @export
#' @examples
#' \dontrun{
#' ToDo
#' }
#'
plot_thickness.mswc.norg <- function(soil, ...){
  p <- plot_scatter(
      soil$data,
      dict(soil, "thickness"),
      dict(soil, "mswc"),
      label= dict(soil, "name"),
      xlab= "Soil thickness (cm)",
      ylab= "Soil maximum water content (mm)",
      legend_colour= "Norg",
      add_geomArgs=list(mapping=ggplot2::aes(colour=!!dict(soil, "norg"))),
      ...
      )

  return(p)
}
glob.chars$thickness.mswc.norg <- c("thickness", "mswc", "name", "norg")

dict <- function(data.object, char.name){
  return(data.object$dict[[char.name]])
}

# get_char <- function(data.object, char.name){
#   eval(dict(data.object, char.name), data.object$data)
# }

plot_limiting.temperatures <- function(weather, histogram=NULL, ...){
  # create data frame with required information
  data <- weather$data %>%
    dplyr::group_by( situation ) %>%
    dplyr::summarise(
      nb_below_0 = sum( !! dict(weather, "Tmin") < 0 ),
      nb_above_35 = sum( !! dict(weather, "Tmax") > 35 ),
      year = unique( !! dict(weather, "Year") ),
      site = unique( !! dict(weather, "Site") )
    )

  # create and return plot
  if(is.null(histogram)){
    histogram <- if(nrow(data)>100) TRUE else FALSE
  }
  if(histogram)
    p <- plot_scatter(
      data,
      "nb_below_0",
      "nb_above_35",
      add_geomArgs = list(mapping=ggplot2::aes(shape= as.factor(year), colour=as.factor(site))),
      xlab="nb days Tmin < 0°C",
      ylab="nb days Tmax > 35°C",
      legend_colour="Site",
      legend_shape="Year",
      ...
    )
  else{
    p <- plot_scatter(
      data,
      "nb_below_0",
      "nb_above_35",
      geom_fun = ggplot2::geom_hex,
      xlab = "nb days Tmin < 0°C",
      ylab = "nb days Tmax > 35°C",
      ...
    )
    situations <- get_hexLabels(data, "nb_below_0", "nb_above_35", c("site", "year"))
    p <- p + aes(label = after_stat(situations))
  }

  return(p)
}
glob.chars$limiting.temeratures <- c("Tmax", "Tmin", "Site", "Year")

get_hexLabels <- function(data, x, y, chars, trunc=8){
  ggplot(data) + aes(!!dplyr::sym(x), !!dplyr::sym(y)) +
    stat_summary_hex(aes(z=(1:nrow(data)), label=after_stat(value)), fun = base::identity, geom="text") -> p

  ggplot_build(p)$data[[1]]$label -> vec

  data[vec[[1]],]

  selection <- df %>% dplyr::ungroup() %>% dplyr::select(all_of(chars))

  vec %>% lapply(function(x) selection[x,]) %>% lapply(apply, 1, paste, collapse=", ") %>%
    lapply(function(x) if(length(x)<=trunc) x else c(x[1:trunc],"...")) %>% sapply(paste, collapse="; ")
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

`geomTextRepel<-` <- function(plot, value){
  plot$layers[[gginnards::which_layers(p, "GeomTextRepel")]] <- value
  return(plot)
}

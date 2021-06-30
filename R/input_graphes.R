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
#' @importFrom magrittr %$%
plot_thickness.mswc.norg <- function(soil, interactive=FALSE, ...){

  p <- soil$dict %$%
    plot_scatter(
      soil$data,
      thickness,
      mswc,
      label= if(interactive) NULL else name,
      xlab= "Soil thickness (cm)",
      ylab= "Soil maximum water content (mm)",
      size.legend= "Norg",
      add.mapping=ggplot2::aes(size=!!norg),
      ...
      )

  if(interactive){
    p <- plotly::ggplotly(p)
    p <- plotly::style(p, text = get_char(soil, "name"),
                       hoverinfo = 'text')
  }
  return(p)
}
glob.chars$thickness.mswc.norg <- c("thickness", "mswc", "name", "norg")

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
plot_scatter <- function(df, x, y, title=NULL, label=NULL, xlab=NULL, ylab=NULL,
                         colour.legend=NULL, shape.legend=NULL, size.legend=NULL, add.mapping=NULL, ...){

  geom_args <- list(...)
  if(!is.null(add.mapping)) geom_args <- combine_aesthetic(geom_args, add.mapping)

  p <- ggplot2::ggplot(df) + ggplot2::aes(x=!!x, y=!!y)

  if(!is.null(colour.legend)) p <- p + ggplot2::labs(colour=colour.legend)
  if(!is.null(shape.legend)) p <- p + ggplot2::scale_shape(shape.legend)
  if(!is.null(size.legend)) p <- p + ggplot2::labs(size=size.legend)
  if(!is.null(label)) p <- p + ggplot2::aes(label=!!label) + ggrepel::geom_text_repel()
  if(!is.null(xlab)) p <- p + ggplot2::xlab(xlab)
  if(!is.null(ylab)) p <- p + ggplot2::ylab(ylab)
  if(!is.null(title)) p <- p + ggplot2::ggtitle(title)

  p <- p + do.call(ggplot2::geom_point, geom_args) + ggplot2::scale_size_area()

  return(p)
}

combine_aesthetic <- function(geom_args, aes){
  geom_args$mapping <- combine.lists(aes, geom_args$mapping)
  return(geom_args)
}

combine.lists <- function(list1, list2){
  # Combine lists 'list1' and 'list2', giving precedence to elements found in 'list1':
  # that is, if $something is found in both 'list1' and 'list2',
  # the new (output) list will have the same values as 'list1' in $something

  if(is.null(list2))
    return(list1)

  list1.names <- names(list1)
  list2.names <- names(list2)

  new.list <- list2

  tmp <- match(list1.names, list2.names)
  w <- which(!is.na(tmp))

  if (length(w) > 0){
    # take values from list1 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list1[[w]]

    # append elements of 'list1' with unmatched names
    new.list <- modifyList(list1[-w], new.list)

  }
  else{
    new.list <- modifyList(list1, new.list)
  }

  return(new.list)
}

get_char <- function(data.object, char.name){
  return(data.object$data[[data.object$dict[[char.name]]]])
}

`geomTextRepel<-` <- function(plot, value){
  plot$layers[[gginnards::which_layers(p, "GeomTextRepel")]] <- value
  return(plot)
}

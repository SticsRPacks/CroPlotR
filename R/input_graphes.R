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
plot_thickness.mswc.norg <- function(name, thickness, mswc, norg, max.overlaps=Inf){
  plot_scatter(thickness, mswc, ggplot2::aes(size=norg), label= paste0(name), xlab= "Soil thickness (cm)",
               ylab= "Soil maximum water content (mm)", size.legend= "Norg", max.overlaps= max.overlaps)
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
plot_scatter <- function(x, y, ...,  title=NULL, label=NULL, max.overlaps= Inf, xlab=NULL, ylab=NULL,
                         colour.legend=NULL, shape.legend=NULL, size.legend=NULL){
  p <- ggplot2::ggplot() + ggplot2::aes(x=x, y=y)
  if(!is.null(colour.legend)) p <- p + ggplot2::labs(colour=colour.legend)
  if(!is.null(shape.legend)) p <- p + ggplot2::scale_shape(shape.legend)
  if(!is.null(size.legend)) p <- p + ggplot2::labs(size=size.legend)
  if(!is.null(label)) p <- p + ggplot2::aes(label=label) + ggrepel::geom_text_repel(box.padding = 0.5, max.overlaps = max.overlaps)
  if(!is.null(xlab)) p <- p + ggplot2::xlab(xlab)
  if(!is.null(ylab)) p <- p + ggplot2::ylab(ylab)
  if(!is.null(title)) p <- p + ggplot2::ggtitle(title)
  p + ggplot2::geom_point(...) + ggplot2::scale_size_area()
}

#' Save CroPlotR plot
#'
#' @description Save the plots to disk
#'
#' @param plot   The plots output from `plot_usms_models()`
#' @param path   The path to the directoy where to save the plots
#' @param suffix A suffix to append to the file name
#' @param width  The plot width
#' @param height The plot heigth
#' @param units  The units for plot width and heigth in `units ("in", "cm", or "mm")`
#' @param dpi    The plot resolution.
#' @param scale  The scaling factor.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
#' (windows only).
#'
#' @details The function uses [ggplot2::ggsave()] under the hood.
#' @return Save the plots to `path`, named by the situation name, and returns the plots invisibly for piping.
#' @export
plot_save= function(plot, path, suffix= "", width = 17, height=12,units="cm",dpi=200,scale = 1.2,device=NULL){

  if(inherits(plot,"ggplot")){
    # plot is just a ggplot
    ggplot2::ggsave(filename = "plot.png", plot = plot,path = path,  width = width,
                    height=height,units=units,dpi=dpi,scale = scale,device = device)
  }

  # plot is a list of plots:
  for(i in seq_along(plot)){
    if(is.null(plot[[i]])){
      next()
    }
    ggplot2::ggsave(filename = paste0(names(plot)[i],suffix,".png"), plot = plot[[i]],
                    path = path,  width = width,height=height,units=units,dpi=dpi,
                    scale = scale, device = device)
  }
  invisible(plot_save)
}

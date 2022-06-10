#' Save CroPlotR plot
#'
#' @description Save the plots to disk
#'
#' @param plot A list of ggplots : output of `plot()`
#' @param out_dir The path to the directory where to save the plots
#' @param suffix A suffix to append to the file name
#' @param width  The plot width
#' @param height The plot heigth
#' @param units  The units for plot width and heigth in `units ("in", "cm", or "mm")`
#' @param dpi    The plot resolution.
#' @param scale  The scaling factor.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
#' (windows only).
#' @param path `r lifecycle::badge("deprecated")` `path` is no
#'   longer supported, use `out_dir` instead.
#'
#' @details The function uses [ggplot2::ggsave()] under the hood.
#' @return Save the plots to `path`, named by the situation name, and returns the plots invisibly for piping.
#' @export
#'
save_plot_png <- function(plot, out_dir, suffix = "", width = 17, height = 12, units = "cm", dpi = 200, scale = 1.2, device = NULL, path = lifecycle::deprecated()) {
  if (lifecycle::is_present(path)) {
    lifecycle::deprecate_warn("0.5.0", "save_plot_png(path)", "save_plot_png(out_dir)")
  } else {
    path <- out_dir # to remove when we update inside the function
  }
  if (inherits(plot, "ggplot")) {
    # plot is just a ggplot
    ggplot2::ggsave(
      filename = "plot.png", plot = plot, path = path, width = width,
      height = height, units = units, dpi = dpi, scale = scale, device = device
    )
  }

  # plot is a list of plots:
  for (i in seq_along(plot)) {
    if (is.null(plot[[i]])) {
      next()
    }
    ggplot2::ggsave(
      filename = paste0(names(plot)[i], suffix, ".png"), plot = plot[[i]],
      path = path, width = width, height = height, units = units, dpi = dpi,
      scale = scale, device = device
    )
  }
  invisible()
}

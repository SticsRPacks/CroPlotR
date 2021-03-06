% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_plot_png.R
\name{save_plot_png}
\alias{save_plot_png}
\title{Save CroPlotR plot}
\usage{
save_plot_png(
  plot,
  path,
  suffix = "",
  width = 17,
  height = 12,
  units = "cm",
  dpi = 200,
  scale = 1.2,
  device = NULL
)
}
\arguments{
\item{plot}{The plots output from \code{plot_usms_models()}}

\item{path}{The path to the directoy where to save the plots}

\item{suffix}{A suffix to append to the file name}

\item{width}{The plot width}

\item{height}{The plot heigth}

\item{units}{The units for plot width and heigth in \verb{units ("in", "cm", or "mm")}}

\item{dpi}{The plot resolution.}

\item{scale}{The scaling factor.}

\item{device}{Device to use. Can either be a device function (e.g. png()), or one of
"eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
(windows only).}
}
\value{
Save the plots to \code{path}, named by the situation name, and returns the plots invisibly for piping.
}
\description{
Save the plots to disk
}
\details{
The function uses \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}} under the hood.
}

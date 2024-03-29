% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aesthetics_scatter.R
\name{aesthetics_scatter}
\alias{aesthetics_scatter}
\title{Manages the aesthetics of the graphics for scatter plots}
\usage{
aesthetics_scatter(
  sim,
  aesthetics = template_aesthetics(),
  overlap = NULL,
  several_sit = FALSE,
  one_version = TRUE,
  iVersion = 1,
  dot_args = NULL,
  is_mixture = FALSE
)
}
\arguments{
\item{sim}{A simulation list of data.frames}

\item{aesthetics}{Template aesthetics list}

\item{overlap}{A list of lists containing the variables to represent on the
same graph when \code{type = "dynamic"}}

\item{several_sit}{Boolean. Must be equal to \code{TRUE} if sim and obs gather
more than one situation and if situations should be differentiated
on the graph.}

\item{one_version}{Boolean. Must be \code{TRUE} if several versions will be
plotted on the same graph.}

\item{iVersion}{Integer. Version number of sim}

\item{dot_args}{List of dot arguments given to plot function}

\item{is_mixture}{Boolean indicating whether the crop is a mixture or not}
}
\value{
An aesthetics expression which will allow to manage the aesthetics
of the graphics
}
\description{
Manages the shape, color and line type of the graphics according
to their content
}
\keyword{internal}

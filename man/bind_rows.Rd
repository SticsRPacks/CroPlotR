% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cropr_bind_split.R
\name{bind_rows}
\alias{bind_rows}
\title{Bind simulation list into dataframe}
\usage{
bind_rows(..., .id = NULL)
}
\arguments{
\item{...}{Simulation outputs in Cropr format, \emph{i.e.} a named list of
\code{data.frame}
for each situation.}

\item{.id}{Name of the column in the new dataframe that identifies the origin
of each row. If \code{...} is a simulation output, it is set to "situation" by
default.}
}
\value{
A single data.frame or tibble binding the rows of all data.Frames or
tibbles included in sim
}
\description{
Bind simulations list with different situations into a single
dataframe
}
\details{
If \code{...} is not of class \code{cropr_simulation}, it uses the regular
function from \code{dplyr}. See \emph{e.g.} \code{\link[SticsRFiles:get_sim]{SticsRFiles::get_sim()}} for an example
output format.
}
\note{
You can perform the same for observations with the following:
\code{bind_rows(obs, .id = "situation")}.
}
\examples{
\dontrun{
# Importing an example with three situations with observation:
workspace <- system.file(file.path("extdata", "stics_example_1"),
  package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(
  usm_path =
    file.path(workspace, "usms.xml")
)
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)

bind_rows(sim)
}
}
\seealso{
split_df2sim
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cropr_bind_split.R
\name{split_df2sim}
\alias{split_df2sim}
\title{Split data.frame into Cropr format}
\usage{
split_df2sim(df, add_cropr_attr = TRUE)
}
\arguments{
\item{df}{A single data.frame or tibble containing simulation results
(as created by \code{bind_rows_sim}).
MUST include \code{Date}and \code{situation} columns.}

\item{add_cropr_attr}{A logical to indicate if the cropr_simulation attribute
must be added to the resulting variable
Set FALSE if you apply the function to observed data,
TRUE otherwise (optional, default value = TRUE).}
}
\value{
A named list of \code{data.frame} for each situation, having the attribute
cropr_simulation.
}
\description{
Split a row-binded data.frame (or tibble) into a Cropr format
simulation list.
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

df <- bind_rows(sim)
split_df2sim(df)
}
}
\seealso{
bind_rows
}

.onAttach <- function(libname, pkgname) {
  required_max_version <- "4.0.0"
  installed_version <- as.character(utils::packageVersion("ggplot2"))

  if (package_version(installed_version) >= package_version(required_max_version)) {
    packageStartupMessage(
      sprintf(
        "/!\  CroPlotR scatter plots may not work properly with ggplot2 %s or higher.\nPlease use ggplot2 3.5.2 or earlier. This issue will be fixed in a future release.",
        installed_version
      )
    )
    packageStartupMessage("")
  }

  packageStartupMessage(
    "Learn CroPlotR at: https://SticsRPacks.github.io/CroPlotR"
  )
  packageStartupMessage("")

  cite <- utils::citation("CroPlotR")
  # Remove underscores around titles (used by R to indicate italics)
  cite_text <- gsub("_([^_]*)_", "\\1", format(cite, style = "text"))

  packageStartupMessage("Please cite CroPlotR in your work:")
  packageStartupMessage(format(cite_text, style = "text"))
}

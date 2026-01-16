.onAttach <- function(libname, pkgname) {
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

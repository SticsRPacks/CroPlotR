text_col <- function(x) {
  # This function is adapted from: https://github.com/tidyverse/tidyverse
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    text_col(paste(
      "Learn CroPlotR at:",
      crayon::blue$underline$bold("https://SticsRPacks.github.io/CroPlotR")
    ))
  )
}

.onLoad <- function(libname, pkgname) {
  required_max_version <- "4.0.0"
  installed_version <- as.character(packageVersion("ggplot2"))

  if (package_version(installed_version) >= package_version(required_max_version)) {
    warning(
      sprintf(
        "CroPlotR scatter plots may not work properly with ggplot2 %s or higher. Please use ggplot2 3.5.2 or earlier. This will be fixed in a future CroPlotR release.",
        installed_version
      ),
      call. = FALSE
    )
  }
}

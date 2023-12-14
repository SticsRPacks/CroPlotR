library(testthat)
library(CroPlotR)
library(vdiffr)

# Force the locale to english to avoid problems with dates:
Sys.setlocale("LC_ALL", "English")

test_check("CroPlotR")

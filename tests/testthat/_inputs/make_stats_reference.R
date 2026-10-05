# Make the reference statistics used in test-stats_reference.R
#
# Run from the package root, on the version of the code taken as reference:
# Rscript tests/testthat/_inputs/make_stats_reference.R

devtools::load_all(".", quiet = TRUE)
library(testthat)
source("tests/testthat/helper-stats_cases.R")

cases <- stats_cases("tests/testthat/_inputs/sim_obs.RData")
reference <- lapply(cases, run_stats_case)

saveRDS(reference, "tests/testthat/_inputs/stats_reference.rds", version = 2)

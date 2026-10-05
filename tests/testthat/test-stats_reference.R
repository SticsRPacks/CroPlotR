# Check that the statistics are the same as the reference ones, made by
# `_inputs/make_stats_reference.R` on the previous version of the code.

reference <- readRDS(test_path("_inputs", "stats_reference.rds"))
cases <- stats_cases()

for (case_name in names(reference)) {
  test_that(paste("statistics are unchanged:", case_name), {
    expect_equal(
      run_stats_case(cases[[case_name]]),
      reference[[case_name]]
    )
  })
}

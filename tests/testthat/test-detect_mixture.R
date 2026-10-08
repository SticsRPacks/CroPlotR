test_that("detect_mixture", {
  sim_data <- data.frame(
    Dominance = c("Principal", "Principal", "Associated", "Associated")
  )
  expect_true(CroPlotR:::detect_mixture(sim_data))

  sim_data <- data.frame(Dominance = c("Single Crop", "Single Crop"))
  expect_false(CroPlotR:::detect_mixture(sim_data))

  sim_data <- data.frame(lai = c(1, 1.2))
  expect_false(CroPlotR:::detect_mixture(sim_data))

  # NA values (e.g. versions bound together, only one with a Dominance column)
  # are not counted as a distinct value:
  sim_data <- data.frame(Dominance = c("0", "0", NA, NA))
  expect_false(CroPlotR:::detect_mixture(sim_data))

  sim_data <- data.frame(Dominance = c("Principal", "Associated", NA))
  expect_true(CroPlotR:::detect_mixture(sim_data))
})

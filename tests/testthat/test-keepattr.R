# Loading the inputs (see test-plot to reproduce the data)
load("_inputs/sim_obs.RData")

test_that("cropr_simulation attribute is kept", {
  expect_s3_class(sim[1], "cropr_simulation")
})

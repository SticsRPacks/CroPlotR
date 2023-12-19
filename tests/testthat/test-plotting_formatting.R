load("_inputs/sim_obs.RData")

test_that("Dynamics plot with unknown variables", {
  expect_error(plot(sim, all_situations = FALSE, var = c("lai_n", "masec", "mafruit")))
})


test_that("Scatter plot with unknown variables", {
  expect_error(plot(sim, obs = obs, type = "scatter", var = c("lai_n", "masec", "mafruit")))
})


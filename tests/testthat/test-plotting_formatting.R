load("_inputs/sim_obs.RData")

test_that("Dynamics plot with wrong args values", {
  expect_error(plot(sim, all_situations = FALSE, var = c("lai_n", "masec", "mafruit")))
  expect_error(plot(sim, all_situations = FALSE, var = c("lai_n"), type = "dynamics"))
  expect_error(plot(sim, all_situations = FALSE, var = c("lai_n"), select_dyn = "obs"))
})


test_that("Scatter plot with wrong args values", {
  expect_error(plot(sim, obs = obs, type = "scatter", var = c("lai_n", "masec", "mafruit")))
  expect_error(plot(sim, type = "scatter", var = c("lai_n"), select_scat = "obs"))
})

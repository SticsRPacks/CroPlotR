workspace <- system.file(file.path("extdata", "stics_example_1"),
  package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(file = file.path(workspace,
                                                          "usms.xml"))
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)
obs <- SticsRFiles::get_obs(workspace = workspace, usm = situations)

test_that("bind_rows", {
  sim_bind <- bind_rows(sim)
  expect_is(sim_bind, "data.frame")
  expect_equal(nrow(sim_bind), 1240)
  expect_equal(
    sort(unique(sim_bind$situation)),
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )
  )
})

test_that("split_df2sim", {
  expect_identical(sim, split_df2sim(bind_rows(sim)))
  # hum for obs results can be in different orders ...
  # (since generally different variables are observed for the different
  # situations and bind_rows gather them ...)
  expect_equal(
    obs$`SC_Pea_2005-2006_N0`,
    split_df2sim(bind_rows(obs, .id = "situation"),
      add_cropr_attr = FALSE
    )$`SC_Pea_2005-2006_N0`[names(obs$`SC_Pea_2005-2006_N0`)]
  )
  expect_equal(
    obs$`SC_Wheat_2005-2006_N0`,
    split_df2sim(bind_rows(obs, .id = "situation"),
      add_cropr_attr = FALSE
    )$`SC_Wheat_2005-2006_N0`[names(obs$`SC_Wheat_2005-2006_N0`)]
  )
  expect_equal(
    obs$`IC_Wheat_Pea_2005-2006_N0`,
    split_df2sim(bind_rows(obs, .id = "situation"),
      add_cropr_attr = FALSE
    )$`IC_Wheat_Pea_2005-2006_N0`[names(obs$`IC_Wheat_Pea_2005-2006_N0`)]
  )
})

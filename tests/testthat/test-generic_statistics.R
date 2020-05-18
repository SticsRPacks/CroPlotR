context("Generic statistics")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)

test_that("format of statistics", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
               formater= format_stics)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),20)
  expect_equal(nrow(df_stats),2)
  expect_equal(df_stats$n_obs,c(16,20))
})


test_that("statistics summary: one group", {
  df_stats= summary(stics_1= sim,obs=obs)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),22)
  expect_equal(nrow(df_stats),6)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),3)
})


test_that("statistics summary: three groups", {
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),22)
  expect_equal(nrow(df_stats),18)
  expect_equal(unique(df_stats$group),paste0("stics_",1:3))
  expect_equal(length(unique(df_stats$situation)),3)
})


context("Generic statistics")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)

test_that("format of statistics", {
  # when computing statistics for each situation one by one
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
               formater= format_stics)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),25)
  expect_equal(nrow(df_stats),2)
  expect_equal(df_stats$n_obs,c(16,20))
})


test_that("statistics with no obs return NULL", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= NULL,
               formater= format_stics)
  expect_true(is.null(df_stats))
})

test_that("statistics summary: one group", {
  df_stats= summary(stics_1= sim,obs=obs,all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),27)
  expect_equal(nrow(df_stats),6)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),3)


})


test_that("statistics summary: three groups", {
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs,all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),27)
  expect_equal(nrow(df_stats),18)
  expect_equal(unique(df_stats$group),paste0("stics_",1:3))
  expect_equal(length(unique(df_stats$situation)),3)
})


test_that("statistics summary: no obs", {
  df_stats= summary(stics_1= sim, obs=NULL, all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),0)

  df_stats= summary(stics_1= sim,stics_2= sim, obs=NULL, all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),0)

  # Observations from one USM are missing only:
  obs$`SC_Wheat_2005-2006_N0`= NULL
  df_stats= summary(stics_1= sim, obs=obs, all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),4)
  expect_equal(unique(df_stats$situation),c("IC_Wheat_Pea_2005-2006_N0","SC_Pea_2005-2006_N0" ))
})

context("Generic statistics")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations, usms_filename = "usms.xml")

test_that("format of statistics", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
               all_situations = FALSE, formater= format_cropr)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),39)
  expect_equal(nrow(df_stats),2)
  expect_equal(df_stats$n_obs,c(8,10))
})


test_that("statistics with no obs return NULL", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= NULL,
               all_situations = FALSE, formater= format_cropr)
  expect_true(is.null(df_stats))
})

test_that("statistics summary: one group", {
  # when computing statistics for each situation one by one
  df_stats= summary(stics_1= sim,obs=obs,all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),41)
  expect_equal(nrow(df_stats),6)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),3)

  # when computing statistics for all situations simultaneously
  df_stats= summary(stics_1= sim,obs=obs,all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),41)
  expect_equal(nrow(df_stats),2)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),1)
})


test_that("statistics summary: three groups", {
  # when computing statistics for each situation one by one
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs,all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),41)
  expect_equal(nrow(df_stats),18)
  expect_equal(unique(df_stats$group),paste0("stics_",1:3))
  expect_equal(length(unique(df_stats$situation)),3)

  # when computing statistics for all situations simultaneously
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs,all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),41)
  expect_equal(nrow(df_stats),6)
  expect_equal(unique(df_stats$group),paste0("stics_",1:3))
  expect_equal(length(unique(df_stats$situation)),1)
})


test_that("statistics summary: no obs", {
  ## when computing statistics for each situation one by one
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


  ## when computing statistics for all situations simultaneously
  df_stats= summary(stics_1= sim, obs=NULL, all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),0)

  df_stats= summary(stics_1= sim,stics_2= sim, obs=NULL, all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),0)

  # Observations from one USM are missing only:
  obs$`SC_Wheat_2005-2006_N0`= NULL
  df_stats= summary(stics_1= sim, obs=obs, all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(nrow(df_stats),2)
  expect_equal(unique(df_stats$situation),c("all_situations" ))
})


test_that("statistical criteria", {
  # Each criterion is stored as a vector in "test_stats.RData"
  df_stats = summary(stics_1= sim,obs=obs)
  df_stats[,-c(1:3,ncol(df_stats))] = round(df_stats[,-c(1:3,ncol(df_stats))],5)
  expect_known_value(df_stats,"stats.test")
})





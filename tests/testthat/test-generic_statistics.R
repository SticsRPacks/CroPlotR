context("Generic statistics")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations, usms_filename = "usms.xml")

test_that("format of statistics", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
               all_situations = FALSE, formater= format_stics)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),39)
  expect_equal(nrow(df_stats),2)
  expect_equal(df_stats$n_obs,c(8,10))
})


test_that("statistics with no obs return NULL", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= NULL,
               all_situations = FALSE, formater= format_stics)
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
  df_stats= summary(stics_1= sim,obs=obs)
  load(file.path(workspace,"test_stats.RData"))
  expect_known_hash(df_stats[["n_obs"]], hash = "3b8c2cc470")
  expect_known_hash(df_stats[["mean_obs"]], hash = "8d97e0cf54")
  expect_known_hash(df_stats[["mean_sim"]], hash = "45c844f7b5")
  expect_known_hash(df_stats[["r_means"]], hash = "eefb462a5b")
  expect_known_hash(df_stats[["sd_obs"]], hash = "ff975de347")
  expect_known_hash(df_stats[["sd_sim"]], hash = "d7e28c0483")
  expect_known_hash(df_stats[["CV_obs"]], hash = "c82ee37293")
  expect_known_hash(df_stats[["CV_sim"]], hash = "9d20b8e677")
  expect_known_hash(df_stats[["R2"]], hash = "766f63f638")
  expect_known_hash(df_stats[["SS_res"]], hash = "03f092f8dc")
  expect_known_hash(df_stats[["Inter"]], hash = "958fc2fa75")
  expect_known_hash(df_stats[["Slope"]], hash = "e6b3b486cd")
  expect_known_hash(df_stats[["RMSE"]], hash = "1d8661f186")
  expect_known_hash(df_stats[["RMSEs"]], hash = "ce08d0d9ee")
  expect_known_hash(df_stats[["RMSEu"]], hash = "6b48e23767")
  expect_known_hash(df_stats[["nRMSE"]], hash = "0b9c9de832")
  expect_known_hash(df_stats[["rRMSE"]], hash = "166c1f387d")
  expect_known_hash(df_stats[["rRMSEs"]], hash = "bc2cc33319")
  expect_known_hash(df_stats[["rRMSEu"]], hash = "ace9f782ab")
  expect_known_hash(df_stats[["pMSEs"]], hash = "e0222659c8")
  expect_known_hash(df_stats[["pMSEu"]], hash = "7517ee6c4c")
  expect_known_hash(df_stats[["Bias2"]], hash = "c481628590")
  expect_known_hash(df_stats[["SDSD"]], hash = "22e70ac171")
  expect_known_hash(df_stats[["LCS"]], hash = "89bf4e22bf")
  expect_known_hash(df_stats[["rbias2"]], hash = "13ecad771b")
  expect_known_hash(df_stats[["rSDSD"]], hash = "3618ec9837")
  expect_known_hash(df_stats[["rLCS"]], hash = "ba87a67964")
  expect_known_hash(df_stats[["MAE"]], hash = "aded86125a")
  expect_known_hash(df_stats[["FVU"]], hash = "809c991dbd")
  expect_known_hash(df_stats[["MSE"]], hash = "576a725ea8")
  expect_known_hash(df_stats[["EF"]], hash = "1bd05f6182")
  expect_known_hash(df_stats[["Bias"]], hash = "5b1202c224")
  expect_known_hash(df_stats[["ABS"]], hash = "aded86125a")
  expect_known_hash(df_stats[["MAPE"]], hash = "4485b624b0")
  expect_known_hash(df_stats[["RME"]], hash = "2c6b33893d")
  expect_known_hash(df_stats[["tSTUD"]], hash = "0a1f48d101")
  expect_known_hash(df_stats[["tLimit"]], hash = "f1615ce225")
  expect_known_hash(df_stats[["Decision"]], hash = "5903c8b509")
})





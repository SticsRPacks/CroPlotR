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
  expect_equal(ncol(df_stats),38)
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
  expect_equal(ncol(df_stats),40)
  expect_equal(nrow(df_stats),6)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),3)

  # when computing statistics for all situations simultaneously
  df_stats= summary(stics_1= sim,obs=obs,all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),40)
  expect_equal(nrow(df_stats),2)
  expect_equal(unique(df_stats$group),"stics_1")
  expect_equal(length(unique(df_stats$situation)),1)
})


test_that("statistics summary: three groups", {
  # when computing statistics for each situation one by one
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs,all_situations=FALSE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),40)
  expect_equal(nrow(df_stats),18)
  expect_equal(unique(df_stats$group),paste0("stics_",1:3))
  expect_equal(length(unique(df_stats$situation)),3)

  # when computing statistics for all situations simultaneously
  df_stats= summary(stics_1= sim,stics_2= sim,stics_3= sim,obs=obs,all_situations=TRUE)
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),40)
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
  expect_equal(n_obs,df_stats[["n_obs"]])
  expect_equal(mean_obs,df_stats[["mean_obs"]])
  expect_equal(mean_sim,df_stats[["mean_sim"]])
  expect_equal(r_means,df_stats[["r_means"]])
  expect_equal(sd_obs,df_stats[["sd_obs"]])
  expect_equal(sd_sim,df_stats[["sd_sim"]])
  expect_equal(CV_obs,df_stats[["CV_obs"]])
  expect_equal(CV_sim,df_stats[["CV_sim"]])
  expect_equal(R2,df_stats[["R2"]])
  expect_equal(SS_res,df_stats[["SS_res"]])
  expect_equal(Inter,df_stats[["Inter"]])
  expect_equal(Slope,df_stats[["Slope"]])
  expect_equal(RMSE,df_stats[["RMSE"]])
  expect_equal(RMSEs,df_stats[["RMSEs"]])
  expect_equal(RMSEu,df_stats[["RMSEu"]])
  expect_equal(nRMSE,df_stats[["nRMSE"]])
  expect_equal(rRMSE,df_stats[["rRMSE"]])
  expect_equal(rRMSEs,df_stats[["rRMSEs"]])
  expect_equal(rRMSEu,df_stats[["rRMSEu"]])
  expect_equal(pRMSEs,df_stats[["pRMSEs"]])
  expect_equal(pRMSEu,df_stats[["pRMSEu"]])
  expect_equal(SDSD,df_stats[["SDSD"]])
  expect_equal(LCS,df_stats[["LCS"]])
  expect_equal(rbias,df_stats[["rbias"]])
  expect_equal(rSDSD,df_stats[["rSDSD"]])
  expect_equal(rLCS,df_stats[["rLCS"]])
  expect_equal(MAE,df_stats[["MAE"]])
  expect_equal(FVU,df_stats[["FVU"]])
  expect_equal(MSE,df_stats[["MSE"]])
  expect_equal(EF,df_stats[["EF"]])
  expect_equal(Bias,df_stats[["Bias"]])
  expect_equal(ABS,df_stats[["ABS"]])
  expect_equal(MAPE,df_stats[["MAPE"]])
  expect_equal(RME,df_stats[["RME"]])
  expect_equal(tSTUD,df_stats[["tSTUD"]])
  expect_equal(tLimit,df_stats[["tLimit"]])
  expect_equal(Decision,df_stats[["Decision"]])
})





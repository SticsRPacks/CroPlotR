context("Generic statistics")

workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)

test_that("format of statistics", {
  df_stats=
    statistics(sim = sim$`IC_Wheat_Pea_2005-2006_N0`, obs= obs$`IC_Wheat_Pea_2005-2006_N0`,
               formater= format_stics)  expect_true(is.list(test_plot))
  expect_true(is.data.frame(df_stats))
  expect_equal(ncol(df_stats),20)
  expect_equal(nrow(df_stats),2)
  expect_equal(df_stats$n_obs,c(16,20))
})


#' workspace= system.file(file.path("extdata", "STICS"), package = "CroPlotR")
#' situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
#' sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
#' obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
#'
#' summary(sim,obs=obs)
#'
test= summary(sim,obs=obs)
attr(test,"class")= NULL
attributes(test)
class(test$Version_1)= NULL


test2= plot(sim,obs=obs)
test2$

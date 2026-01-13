# Loading the inputs (see test-plot to reproduce the data)
load("_inputs/sim_obs.RData")
# load("tests/testthat/_inputs/sim_obs.RData")

# Prepare the data until format_cropr:
all_situations <- TRUE
type <- "dynamic"
var <- "lai_n"
args_list <- CroPlotR:::parse_plot_situations_args(
  list(sim),
  obs = obs, type = type, var = var, all_situations = all_situations
)

situations_outputs <- cat_with_situation(
  args_list$dot_args, args_list$obs, args_list$obs_sd,
  args_list$all_situations, args_list$v_names
)

sit <- args_list$common_situations_models[1]
df_sit <- format_cropr(
  situations_outputs$sim[[sit]],
  situations_outputs$obs[[sit]],
  situations_outputs$obs_sd[[sit]],
  args_list$type,
  args_list$select_dyn,
  args_list$select_scat,
  args_list$successive,
  args_list$reference_var,
  args_list$var,
  args_list$verbose
)


# Apply generic_formatting:
df_all <- generic_formatting(
  df_sit,
  args_list$overlap,
  args_list$situation_group,
  args_list$type, args_list$shape_sit,
  args_list$has_distinct_situations,
  1
)

# Apply tests:
test_that("generic_formatting", {
  expect_s3_class(df_all, "data.frame")
  expect_equal(nrow(df_all), 620)
  expect_equal(unique(df_all$sit_name), "IC_Wheat_Pea_2005-2006_N0")
  expect_equal(unique(df_all$Plant), c("ble", "poi"))
  expect_equal(as.character(min(df_all$Date)), "2005-09-26")
  expect_equal(as.character(max(df_all$Date)), "2006-08-01")
  expect_equal(unique(df_all$version), "Version_1")
  expect_equal(unique(df_all$var), "lai_n")
  expect_equal(mean(df_all$Simulated), 0.21725255, tolerance = 1e-8)
  expect_equal(mean(df_all$Observed, na.rm = TRUE), 0.7625, tolerance = 1e-8)
})

workspace <- system.file(file.path("extdata", "stics_example_1"),
                         package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))
sim <- SticsRFiles::get_sim(workspace = workspace, usm = situations)
obs <- SticsRFiles::get_obs(workspace = workspace, usm = situations)

test_that("cropr_simulation attribute is kept", {
  expect_s3_class(sim[1], "cropr_simulation")
})

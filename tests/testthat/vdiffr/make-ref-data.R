# Make the reference data.
# This is only done once.

workspace <- system.file(file.path("extdata", "stics_example_1"),
    package = "CroPlotR"
)

situations <- SticsRFiles::get_usms_list(
    file = file.path(workspace, "usms.xml")
)

sim <- SticsRFiles::get_sim(
    workspace = workspace,
    usm = situations,
    usms_file = file.path(workspace, "usms.xml")
)

obs <- SticsRFiles::get_obs(
    workspace = workspace,
    usm = situations,
    usms_file = file.path(workspace, "usms.xml")
)

# Rotation example
workspace2 <- system.file(
    file.path("extdata", "stics_example_successive"),
    package = "CroPlotR"
)
situations <- SticsRFiles::get_usms_list(
    file = file.path(workspace2, "usms.xml")
)
sim_rot <- SticsRFiles::get_sim(
    workspace = workspace2,
    usm = situations,
    usms_file = file.path(workspace2, "usms.xml")
)

workspace2 <- system.file(file.path("extdata", "stics_example_2"),
    package = "CroPlotR"
)
sim2 <- SticsRFiles::get_sim(
    workspace = workspace2,
    usms_file = file.path(workspace2, "usms.xml")
)

save(sim, sim2, obs, sim_rot, file = "tests/testthat/_inputs/sim_obs.RData")

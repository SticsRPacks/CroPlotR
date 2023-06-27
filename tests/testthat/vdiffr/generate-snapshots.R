# This script is used by the CI to generate the snapshots.
# It is called twice: once to make the snapshots with the release version
# of CroPlotR, and once to make the snapshots with the current version.
# The snapshots are then compared to check that the plots are identical.

# From https://github.com/r-lib/vdiffr/blob/main/R/expect-doppelganger.R
make_snapshot <- function(name, plot, tmpdir) {
    testthat::local_edition(3)
    fig_name <- vdiffr:::str_standardise(name)
    file <- file.path(tmpdir, paste0(fig_name, ".svg"))
    vdiffr:::write_svg(plot, file, name)

    return(file)
}

if (!exists("pkg_version")) {
    pkg_version <- "Test"
}

if (!exists("tmp")) {
    stop(paste(
        "Please define the temporary folder path in the 'tmp'",
        "object before running this script."
    ))
}

pkg_version <- paste0("_", pkg_version)

load("./tests/testthat/_inputs/sim_obs.RData")

set.seed(1)

# Figure 1 ----------------------------------------------------------------

p <- plot(sim, obs = obs)

make_snapshot(
    paste0("fig.1_simple_dynamic_IC", pkg_version),
    p$`IC_Wheat_Pea_2005-2006_N0`,
    tmp
)

make_snapshot(
    paste0("fig.1_simple_dynamic_Pea", pkg_version),
    p$`SC_Pea_2005-2006_N0`,
    tmp
)

make_snapshot(
    paste0("fig.1_simple_dynamic_Wheat", pkg_version),
    p$`SC_Wheat_2005-2006_N0`,
    tmp
)

# Figure 2 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.2_successive", pkg_version),
    suppressWarnings(
        # suppressWarnings because there is one warning I can't get rid off yet
        plot(
            sim_rot,
            var = c("resmes", "masec_n"),
            successive = list(
                list("demo_Wheat1", "demo_BareSoil2", "demo_maize3")
            )
        )
    ),
    tmp
)

# Figure 3 ----------------------------------------------------------------

p <- plot(sim, obs = obs, overlap = list(list("lai_n", "masec_n")))

make_snapshot(
    paste0("fig.3_overlap_variables_IC", pkg_version),
    p$`IC_Wheat_Pea_2005-2006_N0`,
    tmp
)

make_snapshot(
    paste0("fig.3_overlap_variables_Pea", pkg_version),
    p$`SC_Pea_2005-2006_N0`,
    tmp
)

make_snapshot(
    paste0("fig.3_overlap_variables_Wheat", pkg_version),
    p$`SC_Wheat_2005-2006_N0`,
    tmp
)

# Figure 4 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.4_scatter_all_sitFALSE", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = FALSE
    )$`IC_Wheat_Pea_2005-2006_N0`,
    tmp
)

# Figure 5 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.4_scatter_residues_sitFALSE", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = FALSE
    )$`IC_Wheat_Pea_2005-2006_N0`,
    tmp
)

# Figure 6 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.6_scatter_all_sitTRUE", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = TRUE
    )$all_situations,
    tmp
)

# Figure 7 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.7_res_xvar_lai", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        select_scat = "res",
        all_situations = TRUE,
        reference_var = "lai_n_sim"
    )$all_situations,
    tmp
)


# Figure 8 ----------------------------------------------------------------

set.seed(1)

make_snapshot(
    paste0("fig.8_scatter_txt_shape", pkg_version),
    plot(
        sim,
        obs = obs[c(2, 3)],
        type = "scatter",
        all_situations = TRUE,
        shape_sit = "txt"
    )$all_situations,
    tmp
)

# Figure 9 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.9_scatter_symbol_shape", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = TRUE,
        shape_sit = "symbol"
    )$all_situations,
    tmp
)

# Figure 10 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.10_scatter_symbol_grouped_shape_1", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = TRUE,
        shape_sit = "group",
        situation_group = list(
            list("SC_Pea_2005-2006_N0", "SC_Wheat_2005-2006_N0")
        )
    )$all_situations,
    tmp
)

make_snapshot(
    paste0("fig.10_scatter_symbol_grouped_shape_2", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = TRUE,
        shape_sit = "group",
        situation_group = list(
            "Two Single Crops" = list(
                "SC_Pea_2005-2006_N0",
                "SC_Wheat_2005-2006_N0"
            )
        )
    )$all_situations,
    tmp
)

# Figure 11 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.11_scatter_filter_var", pkg_version),
    plot(
        sim,
        obs = obs,
        type = "scatter",
        all_situations = TRUE,
        var = c("lai_n")
    )$all_situations,
    tmp
)

# Figure 12 ----------------------------------------------------------------

obs_sd <- obs
obs_sd$`SC_Pea_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
    c("Date", "Plant"))] <- 0.05 *
    obs_sd$`SC_Pea_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
        c("Date", "Plant"))]
obs_sd$`SC_Wheat_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
    c("Date", "Plant"))] <- 0.2 *
    obs_sd$`SC_Wheat_2005-2006_N0`[, !(names(obs_sd$`SC_Pea_2005-2006_N0`) %in%
        c("Date", "Plant"))]

make_snapshot(
    paste0("fig.12_scatter_error_bars", pkg_version),
    plot(
        sim,
        obs = obs,
        obs_sd = obs_sd,
        type = "scatter",
        all_situations = TRUE
    )$all_situations,
    tmp
)

# Figure 13 ----------------------------------------------------------------

make_snapshot(
    paste0("fig.13_group_comparison_dynamic", pkg_version),
    plot(sim, sim2, obs = obs, all_situations = FALSE),
    tmp
)

make_snapshot(
    paste0("fig.13_group_comparison_scatter", pkg_version),
    plot(
        "New version" = sim, original = sim2, obs = obs,
        type = "scatter", all_situations = FALSE
    ),
    tmp
)

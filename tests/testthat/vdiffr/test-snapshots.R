# These tests need R version >= 4.2 for testhat >= 3.0.0.
# Also, we only test on the OS and R version the snapshots were built on.

# pkg_version <- "Release"; source('tests/testthat/vdiffr/generate-snapshots.R')
# pkg_version <- "Latest"; source('tests/testthat/vdiffr/generate-snapshots.R')

if (!exists("tmpdir")) {
    stop(paste(
        "Please define the temporary folder path in the 'tmpdir'",
        "object before running this script."
    ))
}

figs <- list.files(tmpdir, pattern = "*fig.*\\.svg$", full.names = TRUE)

latest_figs <- figs[grepl("latest.svg$", figs)]
release_figs <- figs[grepl("release.svg$", figs)]

set.seed(1)

mapply(
    function(latest, release) {
        testthat::test_that(
            paste0("Compare ", basename(latest), " and ", basename(release)),
            {
                testthat::expect_true(
                    testthat::compare_file_text(release, latest)
                )
            }
        )
    },
    latest_figs,
    release_figs
)

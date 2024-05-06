# These tests need R version >= 4.2 for testhat >= 3.0.0.
# Also, we only test on the OS and R version the snapshots were built on.

# snapshots are generated in test-***-plots.R scripts

if (!exists("tmpdir")) {
    stop(paste(
        "Please define the temporary folder path in the 'tmpdir'",
        "object before running this script."
    ))
}

figs <- list.files(tmpdir, pattern = "*fig.*\\.svg$", full.names = FALSE)

latest_figs <- figs[grepl("latest.svg$", figs)]
release_figs <- figs[grepl("release.svg$", figs)]

# grep beginning of latest_figs in release_figs
prefix_latest_figs <- sapply(latest_figs,function(x) substr(x,1,nchar(x)-11))
prefix_release_figs <- sapply(release_figs,function(x) substr(x,1,nchar(x)-12))

missing_figs <- setdiff(prefix_release_figs, prefix_latest_figs)
if (length(missing_figs)>0) {
  message(paste("Warning: figure(s) ",
                paste(missing_figs, collapse = ", "),
                "not generated for latest version.",
                "\n They will therefore not be taken into account in snapshot comparison."))
  release_figs <- paste0(prefix_latest_figs,"-release.svg")
}

release_figs <- file.path(tmpdir, release_figs)
latest_figs <- file.path(tmpdir, latest_figs)

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

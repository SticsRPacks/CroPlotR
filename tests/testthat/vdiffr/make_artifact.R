# This script is used by the CI to generate
# an artifact from the snapshots.

if (!exists("tmp")) {
    stop(paste(
        "Please define the temporary folder path in the 'tmp'",
        "object before running this script."
    ))
}

# These are all figures made by the CI:
figs <- list.files(tmp, pattern = "^fig.*\\.svg$", full.names = TRUE)

# And now we make a zip file with all these figures:
zip_file <- file.path(tempdir(), "vdiffr_artifact.zip")
zip::zip(zip_file, figs)

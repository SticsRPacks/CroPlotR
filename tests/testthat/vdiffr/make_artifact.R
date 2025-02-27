# This script is used by the CI to generate
# an artifact from the snapshots.

if (!exists("tmpdir")) {
  stop(paste(
    "Please define the temporary folder path in the 'tmpdir'",
    "object before running this script."
  ))
}

# These are all figures made by the CI:
figs <- list.files(tmpdir, pattern = "*fig.*\\.svg$", full.names = TRUE)

# And now we make a zip file with all these figures:
zip_file <- file.path(tmpdir, "vdiffr_artifact.zip")
zip::zip(zip_file, figs)

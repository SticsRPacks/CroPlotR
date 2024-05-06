# Documentation on tests for CroPlotR

Several tests on plots have been implemented:

* vdiffr tests that automatically compare plots
* tests that check the content of the ggplot structures

These tests are triggered at every pull request.

## vdiffr tests

The snapshots are generated using the script tests/testthat/test-\*\*-plots.R from the pull request. 

This script is used both with the main branch and the tested branch within the same action, to generate plots in the same software environement and fairly compare them.

The tests/testthat/test-\*\*-plots.R scripts from the main branch are thus not used, only the code of CroPlotR changes between the run on the main branch and the branch to test. This has the benefit that changes to the evaluation script done in the branch to test are always taken into account. The potential drawback is that users that change this script have to know what they are doing, but it should be pretty rare so I think it is worth it.

This test generates an artifact that includes image files of each plot generated on both branches fr visual inspection. It can be downloaded from github ("Actions" page, click on snapshot_comparisn and then on the specific action).

When the tests are locally performed, the figures are generated in the folder _outputs.

## Testing the content of ggplot structures

Expected contents of ggplot structures following the code implemented in CroplotR have been described in the files aesthetics_dynamic.xlsx and aesthetics_scatter.xlsx and implemented for tests in the files tests/testthat/test-\*\*-plots.R.

As many different cases have to be tested for scatter plots, the generation of the different plots is automatically done based on the description given in the file testthat/_inputs/tests_scatter_plots.csv.

## Local tests

When the tests are locally performed, plots are saved in the testhat/_outputs folder for visual comparison (the test must be run on the main branch to have the reference).


# CroPlotR 0.8.0

For users: 

* Update functions names to be homogeneous across SticsRPacks
* Add a method for `[` for cropr_simulation, *i.e.* we can now index the output of plots using `[`
* Deprecate `bind_rows_sim` for `bind_rows`
* Fix issue in scatter plot with `all_situations = FALSE` to get the situation name as hover in plotly
* More documentation
* Compatibility with R >= 4.0.0 + making sure it still works with 3.6.0 <= R < 4.0.0
* Fix issue in `format_cropr`
* Fix issue where points and lines were drawn twice
* Fix issue where points were not colored properly
* Fix issue when overlapping (not grouped properly)
* Fix issue where lines were not colored properly
* Fix issue for plots with residuals + custom x variable
* Fix issue with several versions but only one has common values btw obs and sim

Internals:

* Add golden tests (using `{vdiffr}`)
* Remove SticsRPacks dependency for tests
* Using third edition of testthat
* Format the code using `{styler}` and `{goodpractice}`
* Simplify **a lot** `cat_situations` and make it faaaast
* Use the `.id` in `bind_rows` if provided
* Improved computation of `LCS` to remove a warning 

# CroPlotR 0.7.2

* Added labels so that situation names appear in ggplotly graphs
* Fixed bug with successive argument:

  * did not work with several lists of situations,
  * did not work with observations of only a subset of the situations.
  
* Fixed save_plot_pdf function

# CroPlotR 0.7.1

* Updated README wrt changes in SticsRFiles package
* Fixed position of situation label when using shape_sit = "txt" in scatter plots.

# CroPlotR 0.7.0

* Add `bind_rows_sim` and `split_df2sim` helper functions to either bind a list of simulations into a single `data.frame` or to split it back. Works also on observations.
* `summary` can now be grouped by crops. Useful for cases where we have different usms with different crops, and for intercropping.
* Rename plot saving functions: plot_save -> save_plot_png & save_plot -> save_plot_pdf
* Fix a bug in the legend when plotting two versions in sole crop without observations (only one legend instead of two). See #8039.
* Graphs of residues are no longer plotted with square axes 
* Fix an issue on `extract_plot` for residues plots
* Update documentation
* Several little fixes: added quotes on situation to remove a warning, 

# CroPlotR 0.6.0

 * All crop models wrapped with [CroptimizR](https://github.com/SticsRPacks/CroptimizR) can now automatically use CroPlotR.
  
  For that, the `sim_list` element of the variable returned by the wrapper must have the attribute `cropr_simulation`, please refer to the [guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html) for more details.
 
 * some bugs fixed (coloring for model comparison and overlapping variables, absence of Plant columns in obs and/or sim, ...)

 * license changed to CeCILL-C

# CroPlotR 0.5.0

 * First release version, with dynamic plots, scatter plots, statistics.

# CroPlotR 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

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

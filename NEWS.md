# CroPlotR 0.6.0

 * All crop models wrapped with [CroptimizR](https://github.com/SticsRPacks/CroptimizR) can now automatically use CroPlotR.
  
  For that, the `sim_list` element of the variable returned by the wrapper must have the attribute `cropr_simulation`, please refer to the [guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html) for more details.
 
 * some bugs fixed (coloring for model comparison and overlapping variables, absence of Plant columns in obs and/or sim, ...)

 * license changed to CeCILL-C

# CroPlotR 0.5.0

 * First release version, with dynamic plots, scatter plots, statistics.

# CroPlotR 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

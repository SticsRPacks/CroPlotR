
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CroPlotR

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/CroPlotR/branch/master/graph/badge.svg)](https://codecov.io/gh/SticsRPacks/CroPlotR?branch=master)
[![R build
status](https://github.com/SticsRPacks/CroPlotR/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/CroPlotR/actions)
<!-- badges: end -->

`CroPlotR` aims at the standardization of the process of analyzing the
outputs from crop model such as
[STICS](https://www6.paca.inrae.fr/stics_eng/),
[APSIM](https://www.apsim.info/) or really any model.

> The package is under intensive development and is in a very early
> version. The functions may heavily change from one version to another
> until a more stable version is released.

## Table of Contents

  - [1. Installation](#1-installation)
  - [2. Examples](#2-examples)
      - [2.1 Plotting](#21-plotting)
          - [2.1.1 Dynamic plots](#211-dynamic-plots)
          - [2.1.2 Scatter plots](#212-scatter-plots)
          - [2.1.3 Group comparison](#213-group-comparison)
          - [2.1.4 Plot saving](#214-plot-saving)
          - [2.1.5 Plot extracting](#215-plot-extracting)
      - [2.2 Statistics](#22-statistics)
          - [2.2.1 Dynamic plots](#221-simple-case)
          - [2.2.2 Several groups](#222-several-groups)
          - [2.2.3 Statistics plot](#223-statistics-plot)
      - [3. Help](#3-help)

## 1\. Installation

You can install the released version of CroPlotR from
[Github](https://github.com/SticsRPacks/CroPlotR) either using
`devtools` or the lightweight `remotes` package:

  - With `devtools`

<!-- end list -->

``` r
devtools::install_github("SticsRPacks/CroPlotR@*release")
```

  - With `remotes`

<!-- end list -->

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/CroPlotR@*release")
```

Normally, all the package dependencies will be installed for CRAN
packages.

## 2\. Examples

At the moment, only one function is exported for plots
[`plot()`](https://sticsrpacks.github.io/CroPlotR/reference/plot.stics_simulation.html)
(and its alias `autoplot()`), and one for the statistics
[`summary()`](https://sticsrpacks.github.io/CroPlotR/reference/summary.stics_simulation.html).
These function should be the only one you need for all your plots and
summary statistics.

Here is an example using STICS with a simulation of three situations
(called USM in STICS) with their observations:

  - an intercrop of Wheat and pea
  - a Pea in sole crop
  - a Wheat in sole crop

Let’s import the simulation and observation data:

``` r
library(CroPlotR)
#> Learn CroPlotR at: https://SticsRPacks.github.io/CroPlotR

# Importing an example with three situations with observation:
workspace= system.file(file.path("extdata", "stics_example_1"), package = "CroPlotR")
situations= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
sim= SticsRFiles::get_daily_results(workspace = workspace, usm_name = situations)
obs= SticsRFiles::get_obs(workspace =  workspace, usm_name = situations)
```

### 2.1 Plotting

#### 2.1.1 Dynamic plots

Here is an application of dynamic plots for the 3 situations:

``` r
plot(sim, obs= obs)
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

    #> 
    #> $`SC_Pea_2005-2006_N0`

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

    #> 
    #> $`SC_Wheat_2005-2006_N0`

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

Note that the `obs` argument is explicitly named. This is because the
first argument of the function is `...` (we’ll see why in a minute).

It is possible to choose the situations that we want to plot on the same
graph thanks to the `rotation` parameter. This is particularly useful
when situations follow one another over time.

``` r
sim_rot= sim
lubridate::year(sim_rot$`SC_Wheat_2005-2006_N0`$Date)=lubridate::year(sim_rot$`SC_Wheat_2005-2006_N0`$Date)+1
sim_rot$`SC_Wheat_2005-2006_N0`$ian=sim_rot$`SC_Wheat_2005-2006_N0`$ian+1
plot(sim_rot, obs= obs, rotation = list(list("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")))
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

    #> 
    #> $`SC_Pea_2005-2006_N0 | SC_Wheat_2005-2006_N0 | `

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

We can also overlay variables thanks to the “overlap” parameter with
dynamic plots.

``` r
# Only with single-crop situations
sim_over=sim[c("SC_Pea_2005-2006_N0","SC_Wheat_2005-2006_N0")]
class(sim_over)="stics_simulation"
plot(sim_over, obs= obs, overlap = list(list("lai_n","masec_n")))
#> $`SC_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

    #> 
    #> $`SC_Wheat_2005-2006_N0`

<img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

#### 2.1.2 Scatter plots

Here are the same plots, but presented as scatter plots:

``` r
# Only plotting the first situation for this one:
plots= plot(sim, obs= obs, type = "scatter", all_situations = FALSE)
plots[[1]]
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Residues can also be represented against observations:

``` r
# Only plotting the first situation again:
plots= plot(sim, obs= obs, type = "scatter", select_scat = "res", all_situations = FALSE)
plots[[1]]
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

All these data can also be represented with a single graph for all
situations:

``` r
plots= plot(sim, obs= obs, type = "scatter", all_situations = TRUE)
plots
#> $all_situations
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

#### 2.1.3 Group comparison

We can compare groups of simulations alongside by simply adding the
simulations objects one after the other (that is why the first argument
of the function is `...`). Group simulations can be the results of
simulations from different model versions, or simulations with different
parameter values.

``` r
workspace2= system.file(file.path("extdata", "stics_example_2"), package = "CroPlotR")
sim2= SticsRFiles::get_daily_results(workspace = workspace2)

plot(sim, sim2, obs= obs, type = "scatter", all_situations = FALSE)
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

Here only one plot is outputed because `workspace2` only contains the
intercrop situation.

We can also name the corresponding group in the plot by naming them
while passing to the `plot()` function:

``` r
plot("New version"= sim, original= sim2, obs= obs, type = "scatter", all_situations = FALSE)
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

By default, all variables are returned by `plot()`, but you can filter
them using the `var` argument:

``` r
plot("New version"= sim, original= sim2, obs= obs, type = "scatter", all_situations = FALSE, var=c("lai_n"))
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

#### 2.1.4 Plot saving

The plots can be saved to disk using the `plot_save()` function as
follows:

``` r
plots= plot("New version"= sim, original= sim2, obs= obs, type = "scatter")

plot_save(plot = plots, path = "path/to/directory",suffix = "_scatter")

# or by piping:
plots= plot("New version"= sim, original= sim2, obs= obs, type = "scatter")%>%
  plot_save(., path = "path/to/directory",suffix = "_scatter")
```

#### 2.1.5 Plot extracting

When we have plots with several variables and several situations, the
`extract_plot` function allows to keep the situations and variables that
we need.

In the following example, we want to extract the intercrop situation and
the “masec\_n” variable.

``` r
plots= plot(sim, obs= obs, type = "scatter", all_situations = FALSE)
extract_plot(plots,situations=c("IC_Wheat_Pea_2005-2006_N0"),var=c("masec_n"))
#> $`IC_Wheat_Pea_2005-2006_N0`
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

### 2.2 Statistics

#### 2.2.1 Simple case

Here is an application of summary statistics for the 3 situations:

``` r
summary(sim, obs= obs, all_situations = FALSE)
#> # A tibble: 6 x 27
#>   group situation variable n_obs mean_obs mean_sim sd_obs sd_sim CV_obs CV_sim
#>   <chr> <chr>     <chr>    <int>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 Vers~ IC_Wheat~ lai_n       16    0.762    0.614  0.611  0.532   80.2   86.7
#> 2 Vers~ IC_Wheat~ masec_n     20    3.45     3.30   1.91   2.07    55.3   62.9
#> 3 Vers~ SC_Pea_2~ lai_n        3    2.62     1.74   1.51   1.35    57.7   77.4
#> 4 Vers~ SC_Pea_2~ masec_n      4    5.45     4.38   3.78   3.75    69.4   85.6
#> 5 Vers~ SC_Wheat~ lai_n        3    1.27     1.40   0.440  0.624   34.5   44.5
#> 6 Vers~ SC_Wheat~ masec_n      4    5.39     6.02   3.16   3.96    58.6   65.7
#> # ... with 17 more variables: R2 <dbl>, SS_res <dbl>, RMSE <dbl>, RMSEs <dbl>,
#> #   RMSEu <dbl>, nRMSE <dbl>, rRMSE <dbl>, pRMSEs <dbl>, pRMSEu <dbl>,
#> #   MAE <dbl>, FVU <dbl>, MSE <dbl>, EF <dbl>, Bias <dbl>, ABS <dbl>,
#> #   MAPE <dbl>, RME <dbl>
```

Note that as for the `plot()` function the `obs` argument is explicitly
named. This is because the first argument of the function is `...` to be
able to compare groups (i.e. model versions or simulation with different
parameter values).

And as for the `plot()` function again, it is possible to compute the
statistical criteria for all situations at once.

``` r
summary(sim, obs= obs, all_situations = TRUE)
#> # A tibble: 2 x 27
#>   group situation variable n_obs mean_obs mean_sim sd_obs sd_sim CV_obs CV_sim
#>   <chr> <chr>     <chr>    <int>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 Vers~ all_situ~ lai_n       22     1.09    0.875  0.962  0.781   88.6   89.3
#> 2 Vers~ all_situ~ masec_n     28     4.01    3.84   2.47   2.70    61.5   70.3
#> # ... with 17 more variables: R2 <dbl>, SS_res <dbl>, RMSE <dbl>, RMSEs <dbl>,
#> #   RMSEu <dbl>, nRMSE <dbl>, rRMSE <dbl>, pRMSEs <dbl>, pRMSEu <dbl>,
#> #   MAE <dbl>, FVU <dbl>, MSE <dbl>, EF <dbl>, Bias <dbl>, ABS <dbl>,
#> #   MAPE <dbl>, RME <dbl>
```

#### 2.2.1 Several groups

We can get statistics for each group of simulations by simply adding the
simulations objects one after the other (as for the `plot()` function).

``` r
summary(sim, sim2, obs= obs)
#> # A tibble: 4 x 27
#>   group situation variable n_obs mean_obs mean_sim sd_obs sd_sim CV_obs CV_sim
#>   <chr> <chr>     <chr>    <int>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 Vers~ all_situ~ lai_n       16    0.762    0.614  0.611  0.532   80.2   86.7
#> 2 Vers~ all_situ~ masec_n     20    3.45     3.30   1.91   2.07    55.3   62.9
#> 3 Vers~ all_situ~ lai_n       16    0.762    0.599  0.611  0.423   80.2   70.7
#> 4 Vers~ all_situ~ masec_n     20    3.45     3.32   1.91   3.19    55.3   96.0
#> # ... with 17 more variables: R2 <dbl>, SS_res <dbl>, RMSE <dbl>, RMSEs <dbl>,
#> #   RMSEu <dbl>, nRMSE <dbl>, rRMSE <dbl>, pRMSEs <dbl>, pRMSEu <dbl>,
#> #   MAE <dbl>, FVU <dbl>, MSE <dbl>, EF <dbl>, Bias <dbl>, ABS <dbl>,
#> #   MAPE <dbl>, RME <dbl>
```

We can also name the corresponding group in the plot by naming them
while passing to the `summary()` function:

``` r
summary("New version"= sim, original= sim2, obs= obs)
#> # A tibble: 4 x 27
#>   group situation variable n_obs mean_obs mean_sim sd_obs sd_sim CV_obs CV_sim
#>   <chr> <chr>     <chr>    <int>    <dbl>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 New ~ all_situ~ lai_n       16    0.762    0.614  0.611  0.532   80.2   86.7
#> 2 New ~ all_situ~ masec_n     20    3.45     3.30   1.91   2.07    55.3   62.9
#> 3 orig~ all_situ~ lai_n       16    0.762    0.599  0.611  0.423   80.2   70.7
#> 4 orig~ all_situ~ masec_n     20    3.45     3.32   1.91   3.19    55.3   96.0
#> # ... with 17 more variables: R2 <dbl>, SS_res <dbl>, RMSE <dbl>, RMSEs <dbl>,
#> #   RMSEu <dbl>, nRMSE <dbl>, rRMSE <dbl>, pRMSEs <dbl>, pRMSEu <dbl>,
#> #   MAE <dbl>, FVU <dbl>, MSE <dbl>, EF <dbl>, Bias <dbl>, ABS <dbl>,
#> #   MAPE <dbl>, RME <dbl>
```

By default, all statistics are returned by `summary`, but you can filter
them using the `stat` argument:

``` r
summary("New version"= sim, original= sim2, obs= obs, stat = c("R2","nRMSE"))
#> # A tibble: 4 x 5
#>   group       situation      variable    R2 nRMSE
#>   <chr>       <chr>          <chr>    <dbl> <dbl>
#> 1 New version all_situations lai_n    0.483  58.5
#> 2 New version all_situations masec_n  0.824  24.3
#> 3 original    all_situations lai_n    0.228  70.4
#> 4 original    all_situations masec_n  0.285  74.2
```

Please read the help from the
\[`predictor_assessment()`\]((<https://sticsrpacks.github.io/CroPlotR/reference/predictor_assessment.html>)
function.

#### 2.2.3 Statistics plot

It is also possible to plot the statistics:

In a rather obvious way, the resulting graph will take into account all
the situations simultaneously or not according to the parameter given to
`summary`. Here is an example with `all_situations = FALSE`.

``` r
stats= summary("New version"= sim, original= sim2, obs= obs, stat = c("R2","nRMSE"), all_situations = FALSE)
plot(stats)
```

<img src="man/figures/README-unnamed-chunk-21-1.png" width="100%" /> And
here is an example with `all_situations = TRUE`.

``` r
stats= summary("New version"= sim, original= sim2, obs= obs, stat = c("R2","nRMSE"), all_situations = TRUE)
plot(stats)
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="100%" />

We can choose to plot either the group or the situation in x (and the
other is used for grouping and colouring):

``` r
stats= summary("New version"= sim, original= sim2, obs= obs, stat = c("R2","nRMSE"), all_situations = FALSE)
plot(stats, xvar = "situation", title= "Situation in X")
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="100%" />

## 3\. Help

You can find help for the functions directly using the name of the
function followed by the class of the object you need the method for:

  - plot:

<!-- end list -->

``` r
?plot.stics_simulation

?plot.statistics
```

  - statistics:

<!-- end list -->

``` r
?summary.stics_simulation
```

As soon as other models are implemented, you’ll be able to call their
plotting and statistical methods.

If you have any other problem, please [fill an
issue](https://github.com/SticsRPacks/CroPlotR/issues) on Github.

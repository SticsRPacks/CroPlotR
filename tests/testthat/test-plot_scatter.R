context("testing plot_scatter function")

df <- data.frame(X=(1:3), Y=(2:4), LAB=(3:5), COLOUR=as.factor(4:6), SHAPE=as.factor(5:7))
p <- plot_scatter(df, "X", "Y", title = "TITLE", label="LAB", xlab="XLAB", ylab="YLAB",
                  legend_colour="LEGEND_COLOUR", legend_shape="LEGEND_SHAPE", legend_size="LEGEND_SIZE")

test_that("Plot returns ggplot object", {
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_equal(p$data, df)
})

test_that("Basic plot is working",{
  df <- data.frame(x=(1:3), y=(1:3))
  plot <- plot_scatter(df, "x", "y")
  vdiffr::expect_doppelganger("plot-basic", plot)
})

test_that("Add label is working",{
  set.seed(42)
  df <- data.frame(x=(1:3), y=(1:3), l=(2:4))
  plot <- plot_scatter(df, "x", "y", label="l")
  vdiffr::expect_doppelganger("plot-label", plot)
})

test_that("Add colour is working",{
  df <- data.frame(x=(1:3), y=(1:3), c=as.factor(2:4))
  plot <- plot_scatter(df, "x", "y", add_geomArgs=list(mapping=ggplot2::aes(colour=c)), legend_colour="COLOUR")
  vdiffr::expect_doppelganger("plot-colour", plot)
})

test_that("Add shape is working",{
  df <- data.frame(x=(1:3), y=(1:3), s=as.factor(2:4))
  plot <- plot_scatter(df, "x", "y", add_geomArgs=list(mapping=ggplot2::aes(shape=s)), legend_shape="SHAPE")
  vdiffr::expect_doppelganger("plot-shape", plot)
})

test_that("Add title is working",{
  df <- data.frame(x=(1:3), y=(1:3))
  plot <- plot_scatter(df, "x", "y", title="TITLE")
  vdiffr::expect_doppelganger("plot-title", plot)
})

test_that("Add xlab is working",{
  df <- data.frame(x=(1:3), y=(1:3))
  plot <- plot_scatter(df, "x", "y", xlab="XLAB")
  vdiffr::expect_doppelganger("plot-xlab", plot)
})

test_that("Add ylab is working",{
  df <- data.frame(x=(1:3), y=(1:3))
  plot <- plot_scatter(df, "x", "y", ylab="YLAB")
  vdiffr::expect_doppelganger("plot-ylab", plot)
})

test_that("Ellipsis is working",{
  df <- data.frame(x=(1:3), y=(1:3), c=as.factor(2:4), s=as.factor(2:4))
  plot <- plot_scatter(df, "x", "y", mapping=ggplot2::aes(color=c, shape=s))
  vdiffr::expect_doppelganger("plot-ellipsis", plot)
})

test_that("Ellipsis priority is working",{
  df <- data.frame(x=(1:3), y=(1:3), c1=as.factor(2:4), c2=as.factor(c(1,1,2)), s=as.factor(2:4))
  expect_warning(plot <- plot_scatter(df, "x", "y", mapping=ggplot2::aes(color=c1, shape=s),
                       add_geomArgs=list(mapping=ggplot2::aes(color=c2, alpha=0.7), size=10)))
  vdiffr::expect_doppelganger("plot-ellipsis-priority", plot)
})

test_that("Gestion of units is working",{
  df <- data.frame(
    x=units::set_units((1:3), m),
    y=units::set_units((1:3), kg),
    c1=units::set_units((2:4), cm)
  )
  plot <- plot_scatter(df, "x", "y", mapping=ggplot2::aes(color=as.factor(c1)))
  vdiffr::expect_doppelganger("plot-unit-gestion", plot)
})


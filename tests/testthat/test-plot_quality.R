load("testdata.Rda")

test_that("density plot yields plot", {
  data <- calculate_timing(testdata)
  plot <- plot_density(data, colname = "turn_duration")
  expect_equal(plot$data, data)
  expect_equal(plot$labels$x, "value")
  expect_equal(plot$labels$y, "density")
})

test_that("scatter plot yields plot", {
  data <- calculate_timing(testdata)
  plot <- plot_scatter(data, colname_x = "turn_duration", colname_y = "transition_time")
  expect_equal(plot$data, data)
  expect_equal(plot$labels$x, "x")
  expect_equal(plot$labels$y, "y")
})

test_that("plot is saved and writes message", {
  expect_message(
    plot_quality(testdata, saveplot = TRUE),
    "quality-panel-all.png")
  expect_true(file.exists("quality-panel-all.png"))
  file.remove("quality-panel-all.png")
  # Rplots.pdf is created by default, remove it
  if(file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

  expect_error(
    plot_quality(testdata, source = "nonexistent"),
    "No data for source nonexistent")
})

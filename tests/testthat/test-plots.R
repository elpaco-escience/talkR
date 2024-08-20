testdata <- ifadv::ifadv

#' Auxiliary function that generates a plot, saves it and returns the filename
#'
#' This is required by testthat::expect_snapshot_file to work with graphics.
#' More information at:
#'
#' https://indrajeetpatil.github.io/intro-to-snapshot-testing/#/writing-test
#'
#' @param plot_function The plotting function to be tested
#' @param path The output filename
#' @param ... Parameters for the plotting function
#'
#' @return The output filename
#'
save_plot <- function(plot_function, path, ...) {
  png(path, width=800, height=350) # Create a file placeholder for the plot,
  p <- plot_function(...) # generate the plot...
  dev.off() # ... export it as png and close connection

  # Snapshot requires the tested functions to return a path
  return(path)
}

test_that("Plot quality", {
  path <- "plot_quality.png"

  expect_snapshot_file(
    save_plot(plot_quality, path, testdata),
    path
  )

  on.exit(file.remove(path)) # Clean afterwards
})

# test_that("Plot conversations", {
#   p <- data |>
#     dplyr::filter(source == "/dutch2/DVA9M") |>
#     dplyr::filter(end < 60000) |>
#     ggplot2::ggplot(aes(x = end, y = participant)) +
#     geom_turn(aes(
#       begin = begin,
#       end = end)) +
#     xlab("Time (ms)") +
#     ylab("") +
#     theme_turnPlot()
# })

# test_that("File comparison works", {
#
#   # An auxiliary function returning the path is required
#   wrapper <- function(path) {
#     create_file(path)
#     return(path)
#   }
#
#   expect_snapshot_file(wrapper("object.png"), "object.png")
#
#   # Based on https://indrajeetpatil.github.io/intro-to-snapshot-testing/#/writing-test
# })

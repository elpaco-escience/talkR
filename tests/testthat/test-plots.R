testdata <- ifadv::ifadv

#' Auxiliary decorator
#'
#' This decorator extends the functionality of any plotting function with:
#'
#' 1. Saving the plot to a png file
#' 2. Returning the path
#'
#' This is required by testthat::expect_snapshot_file to work with graphics.
#' More information at:
#'
#' https://indrajeetpatil.github.io/intro-to-snapshot-testing/#/writing-test
#'
#' @param plot_function The plotting function to be tested
#' @param path The output filename
#' @param width (optional)
#' @param height (optional)
#'
#' @return The output filename
#'
with_save <- function(plot_function, path, width=800, height=350) {

  decorated <- function(...) {
    png(path, width, height) # Create a file placeholder for the plot,
    p <- plot_function(...) # generate the plot...
    dev.off() # ... export it as png and close connection

    return(path)
  }

  return(decorated)
}

test_that("Plot quality", {
  path <- "plot_quality.png"
  plot_quality_with_save <- with_save(plot_quality, path)

  expect_snapshot_file(
    plot_quality_with_save(testdata),
    path
  )

  on.exit(file.remove(path)) # Clean afterwards
})

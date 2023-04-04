testthat::test_that("`create_turn_plot()` works as expected", {
  fig_ <- create_turn_plot()
  vdiffr::expect_doppelganger("create turn plot", fig_)
})

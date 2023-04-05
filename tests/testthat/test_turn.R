testthat::test_that("`create_turn_plot()` works as expected", {
  fig_ <- talkr::create_turn_plot()
  vdiffr::expect_doppelganger("create turn plot", fig_)
})


testthat::test_that("`create_turn_plot_modified()` works as expected", {
  fig_ <- talkr::create_turn_plot_modified()
  vdiffr::expect_doppelganger("create turn plot modified", fig_)
})

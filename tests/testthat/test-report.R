## set up the test environment
data <- get_ifadv()

test_that("summary reports are accurate", {
  expect_snapshot(
    report_stats(
      data
    )
  )
})

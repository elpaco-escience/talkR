## Load the test data
testdata <- get_ifadv()

test_that("summary reports are accurate", {
  expect_snapshot(
    report_stats(
      testdata
    )
  )
})

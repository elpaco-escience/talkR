## set up the test environment
data <- ifadv::ifadv

test_that("summary reports are accurate", {
  expect_snapshot(
    report_stats(
      data
    )
  )
})

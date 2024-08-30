## Load the test data
testdata <- get_ifadv()

test_that("summary reports are accurate", {
  testdata <- get_ifadv(destfile = "../../data/ifadv.rda")
  expect_snapshot(
    report_stats(
      testdata
    )
  )
})

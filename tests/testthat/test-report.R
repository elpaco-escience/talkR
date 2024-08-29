## set up the test environment
get_ifadv(destfile = "../../data/ifadv.rda")
testdata <- ifadv

test_that("summary reports are accurate", {
  expect_snapshot(
    report_stats(
      testdata
    )
  )
})

load("testdata.Rda")
testdata <- init(testdata)


test_that("Timing calculation is correct", {
  expect_snapshot(
    calculate_timing(testdata)
  )
})

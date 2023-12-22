load("testdata.Rda")
data <- init(testdata)

test_that("token columns are created", {
  expect_snapshot(tokenize(data))
})

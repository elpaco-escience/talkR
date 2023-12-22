load("testdata.Rda")
data <- init(testdata)

test_that("token columns are created, dataset matches", {
  tx <- tokenize(data)
  expect_true("token" %in% colnames(tx))
  expect_true("uid" %in% colnames(tx))
  expect_true("nwords" %in% colnames(tx))
  expect_true("relative_time" %in% colnames(tx))
  expect_equal(nrow(tx), 770)
  expect_equal(ncol(tx), 6)
})

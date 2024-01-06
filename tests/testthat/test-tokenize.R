load("testdata.Rda")
data <- init(testdata)

test_that("token columns are created, dataset matches", {
  tx <- tokenize(data)
  expect_true("token" %in% colnames(tx))
  expect_true("uid" %in% colnames(tx))
  expect_true("nwords" %in% colnames(tx))
  expect_true("relative_time" %in% colnames(tx))
  expect_true("order" %in% colnames(tx))
  expect_equal(nrow(tx), 738)
  expect_equal(ncol(tx), 7)

  expect_equal(tx$relative_time[1:5], c(315271, 315796, 316320, 315414, 316067))
  expect_equal(tx$token[1:5], c("high", "level", "eh?", "sí", "que"))
  expect_equal(tx$order[1:4], c("first", "middle", "last", "only"))
})

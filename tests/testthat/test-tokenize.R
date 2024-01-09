load("testdata.Rda")
data <- init(testdata)

test_that("token columns are created, dataset matches", {
  tx <- tokenize(data)
  expect_true("token" %in% colnames(tx))
  expect_true("uid" %in% colnames(tx))
  expect_true("nwords" %in% colnames(tx))
  expect_true("relative_time" %in% colnames(tx))
  expect_true("order" %in% colnames(tx))
  expect_true("rank" %in% colnames(tx))
  expect_true("frequency" %in% colnames(tx))
  expect_equal(nrow(tx), 738)

  expect_equal(tx$relative_time[1:5], c(315271, 315796, 316320, 315414, 316067))
  expect_equal(tx$token[1:5], c("high", "level", "eh?", "sí", "que"))
  expect_equal(tx$order[1:4], c("first", "middle", "last", "only"))
  expect_equal(tx$rank[1:5], c(243, 307, 70, 50, 14))
})

test_that("no issues arise with dataset containing existing nwords column", {
  data$nwords <- 1
  tx <- tokenize(data)
  expect_equal(tx$nwords[1:5], c(3, 3, 3, 1, 6))
})

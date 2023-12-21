data <- data.frame(
  begin = c(100000, 200000, 300000),
  end = c(200000, 300000, 400000),
  participant = c("A", "B", "A"),
  utterance = c("hello", "world", "hello"),
  source = c("A.txt", "A.txt", "A.txt"),
  uid = c("a", "b", "c")
)

test_that("talkr verification checks columns", {
  expect_no_error(check_talkr(data))

  data$uid <- NULL
  expect_error(check_talkr(data), "Column `uid` was not found in the dataset.")
})


test_that("verification finds errors in timing", {
  data$begin <- c(100,200,300)
  data$end <- c(200,300,400)
  warnings <- capture_warnings(check_talkr(data))
  expect_match(warnings, "column `begin` is in milliseconds", all = FALSE)
  expect_match(warnings, "column `end` is in milliseconds", all = FALSE)

  data$begin <- c("a", "b", "c")
  expect_error(check_talkr(data), "`begin` must be numeric")
})

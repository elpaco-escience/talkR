test_that("talkr verification checks columns", {
  data <- data.frame(
    begin = c(1, 2, 3),
    end = c(2, 3, 4),
    # participant = c("A", "B", "A"),
    utterance = c("hello", "world", "hello"),
    source = c("A.txt", "A.txt", "A.txt")
  )
  expect_error(check_talkr(data), "Column `participant` was not found in the dataset.")

})

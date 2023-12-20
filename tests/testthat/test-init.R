dummy_data <- data.frame(
  col1 = c(1,2,3,4,5),
  col2 = c(6,7,8,9,10),
  x = c("a","a","b","b","b"),
  y = c("c","c","c","d","d"),
  src = c("A.txt", "A.txt", "A.txt", "A.txt", "A.txt")
)


test_that("talkr dataset initialized with minimal columns", {
  talkr_dataset <- init(dummy_data,
                        source = "src",
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y")

  expect_equal(talkr_dataset$source, dummy_data$src)
  expect_equal(talkr_dataset$begin, dummy_data$col1)
  expect_equal(talkr_dataset$end, dummy_data$col2)
  expect_equal(talkr_dataset$participant, dummy_data$x)
  expect_equal(talkr_dataset$utterance, dummy_data$y)
})


test_that("necessary columns are checked in the dataset", {
  expect_error(init(dummy_data),
               "not found in the dataset")

  expect_error(init(dummy_data,
                    source = "src",
                    begin = "col1",
                    end = "does not exist",
                    participant = "x",
                    utterance = "y"),
               "`does not exist` was not found in the dataset")
})

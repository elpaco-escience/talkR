dummy_data <- data.frame(
  col1 = c(1,2,3,4,5),
  col2 = c(6,7,8,9,10),
  x = c("a","a","b","b","b"),
  y = c("c","c","c","d","d")
)

talkr_dataset <- init(dummy_data,
                      begin = "col1",
                      end = "col2",
                      participant = "x",
                      utterance = "y")

test_that("talkr dataset initialized with minimal columns", {
  expect_equal(talkr_dataset$begin, dummy_data$col1)
  expect_equal(talkr_dataset$end, dummy_data$col2)
  expect_equal(talkr_dataset$participant, dummy_data$x)
  expect_equal(talkr_dataset$utterance, dummy_data$y)
})



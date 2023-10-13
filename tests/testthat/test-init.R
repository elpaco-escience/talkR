dummy_data <- data.frame(
  col1 = c(1,2,3,4,5),
  col2 = c(6,7,8,9,10),
  x = c("a","a","b","b","b"),
  y = c("c","c","c","d","d")
)



test_that("talkr dataset initialized with minimal columns", {
  talkr_dataset <- init(dummy_data,
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y")

  expect_equal(talkr_dataset$begin, dummy_data$col1)
  expect_equal(talkr_dataset$end, dummy_data$col2)
  expect_equal(talkr_dataset$participant, dummy_data$x)
  expect_equal(talkr_dataset$utterance, dummy_data$y)
})


test_that("timestamps are converted to milliseconds", {
  talkr_dataset <- init(dummy_data,
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y",
                        timeunit = "s")

  expect_equal(talkr_dataset$begin, dummy_data$col1 * 1000)

  talkr_dataset <- init(dummy_data,
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y",
                        timeunit = "m")

  expect_equal(talkr_dataset$end, dummy_data$col2 * 60000)
})

test_that("faulty inputs are dealt with", {
  expect_error(init(dummy_data,
                    begin = "col1",
                    end = "col2",
                    participant = "x",
                    utterance = "y",
                    timeunit = "h"),
               "timeunit must be one of 'ms', 's', or 'm'")
})

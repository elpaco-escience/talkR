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


test_that("timestamp_to_milliseconds works", {
  expect_equal(timestamp_to_milliseconds("00:00:00.000"), 0)
  expect_equal(timestamp_to_milliseconds("00:00:00.001"), 1)
  expect_equal(timestamp_to_milliseconds("00:00:01.000"), 1000)
  expect_equal(timestamp_to_milliseconds("00:01:00.000"), 60000)
  expect_equal(timestamp_to_milliseconds("00:10:00.000"), 600000)
  expect_equal(timestamp_to_milliseconds("01:00:00.000"), 3600000)
  expect_equal(timestamp_to_milliseconds("10:00:00.000"), 36000000)
  expect_equal(timestamp_to_milliseconds("00:00:10", format = "%H:%M:%S"), 10000)
  expect_equal(timestamp_to_milliseconds("10.000", format = "%OS"), 10000)
  expect_equal(timestamp_to_milliseconds("00_00_10", format = "%H_%M_%S"), 10000)
  expect_equal(timestamp_to_milliseconds("34", format = "%S"), 34000)
  expect_equal(timestamp_to_milliseconds("34.123", format = "%S"), 34000)
  expect_equal(timestamp_to_milliseconds("01:01:01.001", format = "%T"), 3661000)
})

dummy_data_with_time <- data.frame(
  begin = c("00:00:10.001","00:00:14.301","00:00:30.399","00:01:10.003","05:01:40.393"),
  end =c("10.001","14.301","30.399","10.003","40.393"),
  alt = c(1,2,3,4,5),
  participant = c("a","a","b","b","b"),
  utterance = c("c","c","c","d","d"),
  source = c("A.txt", "A.txt", "A.txt", "A.txt", "A.txt")
)

test_that("timestamp conversion is done with init", {
  # conversion is done from %H:%M:%OS to milliseconds
  # end-column has insufficient information and converts to NA
  talkr_dataset <- init(dummy_data_with_time,
                        format_timestamps = "%H:%M:%OS")
  expect_equal(talkr_dataset$begin, c(10001, 14301, 30399, 70003, 18100393))
  expect_equal(talkr_dataset$end, rep(as.numeric(NA), 5))

  # conversion is done from %OS to milliseconds
  # begin column has more information, so it is parsed but only hours are used
  # ideally, this would be NA as well; formats must match...
  talkr_dataset <- init(dummy_data_with_time,
                        format_timestamps = "%OS")
  expect_equal(talkr_dataset$end, c(10001, 14301, 30399, 10003, 40393))
  expect_equal(talkr_dataset$begin, c(0,0,0,0,5000))
  dummy_data_with_time$begin <- NULL
  talkr_dataset <- init(dummy_data_with_time,
                        begin = "alt")
  expect_equal(talkr_dataset$begin, dummy_data_with_time$alt)
})

test_that("UIDs are generated correctly", {
  talkr_dataset <- init(dummy_data,
                        source = "src",
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y")

  expected_UIDs <- c("Atxt-0001-1",
                     "Atxt-0002-2",
                     "Atxt-0003-3",
                     "Atxt-0004-4",
                     "Atxt-0005-5")
  expect_equal(talkr_dataset$uid, expected_UIDs)
})

test_that("Warning is generated with existing UID column", {
  dummy_data$uid = c("a", "b", "c", "d", "e")
  expect_warning(
    talkr_dataset <- init(dummy_data,
                      source = "src",
                      begin = "col1",
                      end = "col2",
                      participant = "x",
                      utterance = "y"),
                 "already exists in the dataset")

  expect_true("original_uid" %in% names(talkr_dataset))

})

test_that("init works with source = NULL", {
  expect_no_error(talkr_dataset <- init(dummy_data,
                        source = NULL,
                        begin = "col1",
                        end = "col2",
                        participant = "x",
                        utterance = "y"))
  expect_false("source" %in% names(dummy_data))
  expect_true("source" %in% names(talkr_dataset))
  expected_UIDs <- c("talkr-0001-1",
                     "talkr-0002-2",
                     "talkr-0003-3",
                     "talkr-0004-4",
                     "talkr-0005-5")
  expect_equal(talkr_dataset$uid, expected_UIDs)
})

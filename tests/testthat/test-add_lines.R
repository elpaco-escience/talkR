df <- data.frame(
  source = rep("x", 5),
  begin = c(0, 1000, 2000, 3000, 300001),
  end = c(1000, 2000, 3000, 4000, 301000),
  participant = c("A", "B", "A", "B", "B"),
  utterance = c("hello", "world", "how", "are", "you world")
)
df <- init(df)
df_token <- tokenize(df)

test_that("add_lines creates line IDs and cuts off timing",{
  df_lines <- add_lines(df, line_duration = 1500)
  expect_equal(df_lines$line_id, c(1, 1, 2, 3, 201))
  expect_equal(df_lines$line_begin, c(0, 1000, 500, 0, 1))
})

test_that("No errors with specific durations", {
  expect_no_error(add_lines(df, line_duration = 25))
  expect_no_error(add_lines(df, line_duration = 1000))
})

test_that("add_lines can work with a tokenized data frame", {
  df_token_lines <- add_lines(df_token,
                              time_columns = "relative_time",
                              line_duration = 1500)
  expect_equal(df_token_lines$relative_time,
               c(500, 1500, 2500, 3500, 300251, 300750))
})

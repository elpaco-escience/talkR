load("testdata.Rda")

test_that("applying geometries yields expected datasets", {
  p0 <- testdata |>
    ggplot2::ggplot(ggplot2::aes(x = end, y = participant))

  d0 <- ggplot2::layer_data(p0)
  expect_equal(d0$x, testdata$end)
  expect_equal(max(d0$y), 23)

  p1 <- p0 +
    geom_turn(ggplot2::aes(
      begin = begin,
      end = end))

  d1 <- ggplot2::layer_data(p1)
  expect_equal(d1$end, testdata$end)
  expect_equal(d1$begin, testdata$begin)
  expect_equal(max(d1$y), 23)

  tokens <- tokenize(testdata)

  p2 <- p1 +
  geom_token(data = tokens,
                      ggplot2::aes(x = relative_time,
                          y = participant,
                          color = rank))

  d2 <- ggplot2::layer_data(p2, 2)
  expect_equal(d2$x, tokens$relative_time)
})

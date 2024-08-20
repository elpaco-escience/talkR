## set up the test environment
data <- ifadv::ifadv

test_that("language inspection yields stats", {
  expect_snapshot(cat(
    inspect_language(
      data, lang="dutch"
      ))
  )

})

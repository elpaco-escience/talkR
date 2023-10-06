## set up the test environment
# Install ifadv and devtools only if required
if (!requireNamespace("ifadv")){
  if (!requireNamespace("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("elpaco-escience/ifadv")
}

data <- ifadv::ifadv

test_that("language inspection yields stats", {
  expect_snapshot(cat(
    inspect_language(
      data, lang="dutch"
      ))
  )

})

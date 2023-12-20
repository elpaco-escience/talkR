## set up the test environment
# Install ifadv and devtools only if required
if (!requireNamespace("ifadv")){
  if (!requireNamespace("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("elpaco-escience/ifadv")
}

data <- ifadv::ifadv

test_that("summary reports are accurate", {
  expect_snapshot(
    report_stats(
      data
    )
  )
})

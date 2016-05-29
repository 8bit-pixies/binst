library(binst)

test_that("using mdlp", {
  mdlp_breaks <- create_breaks(1:10, rep(c(1,2), each=5), method="entropy")
  expect_equal(mdlp_breaks, 5.5)
})

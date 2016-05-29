library(binst)

test_that("using decision trees partykit", {
  dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="dt")
  expect_equal(dt_breaks, c(5.4, 6.1, 7.0))
})
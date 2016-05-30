library(binst)

test_that("using earth", {
  dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="earth")
  expect_equal(length(dt_breaks), 5)
})

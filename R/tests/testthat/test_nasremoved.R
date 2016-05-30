library(binst)

test_that("NAs removed in create_breaks correctly", {
  expect_silent(create_breaks(1:10))
  expect_warning(create_breaks(c(1:10, NA)))
  expect_warning(create_breaks(x=c(iris$Sepal.Length, NA), y=c(iris$Species, NA), method="dt"))
})

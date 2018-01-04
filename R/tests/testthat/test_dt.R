library(binst)

test_that("using decision trees partykit", {
  dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="dt")
  expect_equal(length(dt_breaks), 1)
})


test_that("using decision trees partykit with controls works", {
  dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="dt", control=list(na.action=na.pass))
  expect_equal(length(dt_breaks), 1)
})

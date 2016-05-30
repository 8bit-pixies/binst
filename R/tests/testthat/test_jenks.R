library(binst)

test_that("jenks breaks", {
  breaks_jenks <- create_breaks(1:10, method="jenks")
  expect_length(breaks_jenks, 3)
  expect_gt(min(breaks_jenks), min(1:10))
  expect_lt(max(breaks_jenks), max(1:10))
})

test_that("jenks breaks for iris", {
  jenks_breaks <- create_breaks(iris$Sepal.Length, method="jenks")
  expect_gt(min(jenks_breaks), min(iris$Sepal.Length))
  expect_lt(max(jenks_breaks), max(iris$Sepal.Length))
})

test_that("jenks breaks has control params correctly passed", {
  breaks_jenks <- create_breaks(1:10, method="jenks", control=list(k=3))
  expect_length(breaks_jenks, 3)
})

test_that("jenks breaks has control params correctly passed when k is not in it", {
  breaks_jenks <- create_breaks(1:10, method="jenks", control=list(subset=NULL))
  expect_length(breaks_jenks, 3)
})

library(binst)

test_that("kmeans breaks are close enough", {
  breaks_kmeans <- create_breaks(1:10)
  expect_length(breaks_kmeans, 2)
  expect_equal(breaks_kmeans > 3, c(TRUE, TRUE))
  expect_equal(breaks_kmeans < 8, c(TRUE, TRUE))
})

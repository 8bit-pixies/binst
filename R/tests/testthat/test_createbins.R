library(binst)

test_that("Bins are created correctedly for simple example", {
  expect_equal(create_bins(1:10, c(3, 5)), c(1,1,1,2,2,3,3,3,3,3))
  expect_equal(create_bins(1:4, c(2.5)), c(1,1,2,2))
})

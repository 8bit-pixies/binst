library(binst)

test_that("Bins are created correctedly for simple example", {
  expect_equal(create_bins(1:10, c(3, 5)), c(1,1,1,2,2,3,3,3,3,3))
  expect_equal(create_bins(1:4, c(2.5)), c(1,1,2,2))
})

test_that("hinge bins are created", {
  expect_gt(dim(create_bins(1:10, c(3, 5), method="hinge"))[2], 1)
})

test_that("bins throw warning when method is empty", {
  expect_warning(create_bins(1:10, c(3, 5), method=""))
})

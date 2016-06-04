library(binst)

test_that("using earth", {
  earth_breaks <- create_breaks(x=iris$Sepal.Length, y=iris$Sepal.Width, method="earth")
  expect_equal(length(earth_breaks), 4)
})

test_that("using earth with controls", {
  earth_breaks <- create_breaks(x=iris$Sepal.Length, y=iris$Sepal.Width,
                             method="earth", control=list(glm=list(family=gaussian)))
  expect_equal(length(earth_breaks), 4)
})

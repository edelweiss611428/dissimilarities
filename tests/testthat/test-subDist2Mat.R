# tests/testthat/test-subDist2Mat.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)
dx = dist(x)

idx1 = 1:5
idx2 = 2:6

test_that("subDist2Mat gives similar results as as.matrix", {

  subDist2Mat_ = as.vector(subDist2Mat(dx, idx1, idx2))
  as_matrix = as.vector(as.matrix(dx)[idx1,idx2])
  expect_equal(subDist2Mat_, as_matrix)
})

test_that("invalid dist", {

  test_cases = list(F, 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subDist2Mat(test_cases[[i]], idx1, idx2))
  }

})


test_that("invalid idx", {

  test_cases = list(F, "a", 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subDist2Mat(dx, test_cases[[i]], idx2))
    expect_error(subDist2Mat(dx, idx1, test_cases[[i]]))
  }

})


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

test_that("fail if output matrix size > MAX.INT", {
  x_large = as.matrix(rnorm(1000))
  dx_large = fastDist(x_large)
  idx1_large = rep(1:5, 10^5)
  idx2_large = rep(1:5, 10^5)
  expect_error(subDist2Mat(dx_large, idx1_large, idx2_large))

})


test_that("invalid dist", {

  dx1 = dx
  dx1[1] = NA
  test_cases = list(dx1, 1:100, as.matrix(c(1:99, NA)))

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


test_that("subDist2Mat retains row names", {
  set.seed(61)
  X = matrix(rnorm(100), nrow = 10)
  rownames(X) = paste0("X", 1:10)
  dx = fastDist(X)
  subDMat = subDist2Mat(dx, 1:5, 6:8)
  expect_equal(rownames(subDMat), rownames(X)[1:5])
  expect_equal(colnames(subDMat), rownames(X)[6:8])

  X = matrix(rnorm(100), nrow = 10)
  dx = fastDist(X)
  subDMat = subDist2Mat(dx, 1:5, 6:8)
  expect_equal(rownames(subDMat), as.character(1:5))
  expect_equal(colnames(subDMat), as.character(6:8))

})




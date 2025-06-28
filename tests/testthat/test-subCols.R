# tests/testthat/test-subCols.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)
dx = dist(x)

idx = 1:5

test_that("subCol gives similar results as as.matrix", {

  subCols_ = as.vector(subCols(dx, idx))
  as_matrix = as.vector(as.matrix(dx)[,idx])
  expect_equal(subCols_, as_matrix)
})


test_that("fail if output matrix size > MAX.INT", {
  x_large = as.matrix(rnorm(1000))
  dx_large = fastDist(x_large)
  idx_large = rep(1:5, 10^6)
  expect_error(subCols(dx_large, idx_large))

})


test_that("invalid dist", {
  x_large = as.matrix(rnorm(10^3))
  idx_large = rep(1:5, 10^6)
  dx_large = fastDist(x_large)
  expect_error(subCols(dx_large, idx_large))
})


test_that("invalid idx", {

  test_cases = list(F, "a", 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subCols(dx, test_cases[[i]]))
  }

})



test_that("subCols retains row names", {
  set.seed(61)
  X = matrix(rnorm(100), nrow = 10)
  rownames(X) = paste0("X", 1:10)
  dx = fastDist(X)
  subDx = subCols(dx, 1:5)
  expect_equal(rownames(subDx), rownames(X))
  expect_equal(colnames(subDx), rownames(X)[1:5])

  X = matrix(rnorm(100), nrow = 10)
  dx = fastDist(X)
  subDx = subCols(dx, 1:5)
  expect_equal(rownames(subDx), as.character(1:10))
  expect_equal(colnames(subDx), as.character(1:5))
})










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

test_that("invalid dist", {

  test_cases = list(F, 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subCols(test_cases[[i]], idx))
  }

})


test_that("invalid idx", {

  test_cases = list(F, "a", 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subCols(dx, test_cases[[i]]))
  }

})


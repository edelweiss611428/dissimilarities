# tests/testthat/test-Dist2Mat.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)
dx = dist(x)


test_that("Dist2Mat gives similar results as as.matrix", {

  D2M = as.vector(Dist2Mat(dx))
  as_matrix = as.vector(as.matrix(Dist2Mat(dx)))
  expect_equal(D2M, as_matrix)
})


test_that("Dist2Mat gives similar results as as.matrix", {

  D2M = as.vector(Dist2Mat(dx))
  as_matrix = as.vector(as.matrix(Dist2Mat(dx)))
  expect_equal(D2M, as_matrix)
})


test_that("invalid dist", {
  dx1 = dx
  dx1[1] = NA
  test_cases = list(dx1, 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(Dist2Mat(test_cases[[i]]))
  }
})






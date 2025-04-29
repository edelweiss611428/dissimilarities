# tests/testthat/test-subDist2Dist.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)
dx = dist(x)
idx = 1:5
test_that("subDist2Dist gives similar results as as.matrix", {

  subDist2Dist_ = as.vector(subDist2Dist(dx, idx))
  as_matrix = as.vector(as.dist(as.matrix(dx)[idx,idx]))
  expect_equal(subDist2Dist_, as_matrix)
})

test_that("invalid dist", {

  test_cases = list(F, 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subDist2Dist(test_cases[[i]], idx))
  }

})


test_that("invalid idx", {

  test_cases = list(F, "a", 1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(subDist2Dist(dx, test_cases[[i]]))
  }

})


test_that("invalid (diag, upper) option", {

  test_cases = list(NA, "a", 123)

  for(i in seq_along(test_cases)){
    expect_error(subDist2Dist(dx, 1:5, diag = test_cases[[i]]))
    expect_error(subDist2Dist(dx, 1:5, upper = test_cases[[i]]))
  }

})







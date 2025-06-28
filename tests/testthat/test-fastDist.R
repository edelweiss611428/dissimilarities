# tests/testthat/test-fastDist.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)

test_that("fastDist gives similar results as stats::dist", {
  methods = c("euclidean", "manhattan", "canberra", "maximum", "minkowski", "minkowski")
  ps = c(rep(2,4),10,100)

  for(i in seq_along(methods)){
    fastDist_ = as.vector(fastDist(x, methods[i], p = ps[i]))
    statsdist_ = as.vector(stats::dist(x, methods[i], p = ps[i]))
    expect_equal(fastDist_, statsdist_)
  }
})


test_that("fastDist gives similar results as proxy::dist", {
  methods = "cosine"

  fastDist_ = as.vector(fastDist(x, methods))
  proxydist_ = as.vector(proxy::dist(x, methods))
  expect_equal(fastDist_, proxydist_)

})

test_that("fail if nrow(X) > 65535", {
  x_large = as.matrix(rnorm(10^6))
  expect_error(fastDist(x_large))
})



test_that("invalid method", {

  test_cases = list(c("Sydney", "Melbourne", "Brisbane"),
                    T, F, NA, 22)

  for(i in seq_along(test_cases)){
    expect_error(fastDist(x, test_cases[[i]]))
  }

})

test_that("invalid p", {
  test_cases = list(NA, "a", NULL, T)

  for(i in seq_along(test_cases)){
    expect_error(fastDist(x, p = test_cases[[i]]))
  }

})


test_that("data contain NA | not numeric matrix", {

  test_cases = list(1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(fastDist(test_cases[[i]]))
  }

})

test_that("invalid (diag, upper) option", {

  test_cases = list(NA, "a", 123)

  for(i in seq_along(test_cases)){
    expect_error(fastDist(x, diag = test_cases[[i]]))
    expect_error(fastDist(x, upper = test_cases[[i]]))
  }

})


test_that("fastDist retains row names", {
  set.seed(59)
  x = matrix(rnorm(100), nrow = 10)
  rownames(x) = paste0("X", 1:10)
  dx = fastDist(x)
  expect_equal(attr(dx, "Labels"), rownames(x))

  x = matrix(rnorm(100), nrow = 10)
  rownames(x) = as.character(1:10)
  dx = fastDist(x)
  expect_equal(attr(dx, "Labels"), rownames(x))
})










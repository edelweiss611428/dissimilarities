# tests/testthat/test-expandDist.R
set.seed(4000)
A = matrix(rnorm(100), nrow = 10)
B = matrix(rnorm(100), nrow = 10)
AB = rbind(A,B)


canberra_dist = function(x,y){
  nx = nrow(x)
  ny = nrow(y)
  dx = matrix(numeric(nx*ny), nrow = nx)
  for(i in 1:nx){
    for(j in 1:ny){
      dx[i,j] = stats::dist(rbind(x[i,],y[j,]), "canberra")
    }
  }
  return(dx)
}

test_that("expandDist gives similar results as fastDist", {
  methods = c("euclidean", "manhattan", "maximum", "minkowski", "minkowski", "canberra")
  #canberra distance not included
  ps = c(rep(2,4),10,100)

  for(i in seq_along(methods)){
    distA = fastDist(A, methods[i], p = ps[i])
    expandDist_ = as.vector(expandDist(distA, A, B, method = methods[i], p = ps[i]))
    fastDist_ = as.vector(fastDist(AB, methods[i], p = ps[i]))
    expect_equal(expandDist_, fastDist_)
  }
})


test_that("fail if nrow(A)*nrow(B) > MAX.INT", {
  x_large = as.matrix(rnorm(10^4))
  y_large = as.matrix(rnorm(10^6))
  dx_large = fastDist(x_large)
  expect_error(expandDist(dx_large, x_large, y_large))
})

test_that("fail if different ncol", {
  x_ = matrix(rnorm(100), nrow = 20)
  y_ = matrix(rnorm(100), nrow = 5)
  dx_ = fastDist(x_)
  expect_error(expandDist(dx_, x_, y_))
})


test_that("invalid method", {
  distA = fastDist(A)

  test_cases = list(c("Sydney", "Melbourne", "Brisbane"),
                    T, F, NA, 22)

  for(i in seq_along(test_cases)){
    expect_error(expandDist(distA, A,B, test_cases[[i]]))
  }

})

test_that("invalid p", {
  test_cases = list(NA, "a", NULL, T)
  distA = fastDist(A)

  for(i in seq_along(test_cases)){
    expect_error(expandDist(distA, A,B, p = test_cases[[i]]))
  }

})


test_that("data contain NA | not numeric matrix", {
  distA = fastDist(A)
  test_cases = list(1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(expandDist(distA, test_cases[[i]],B))
  }
})

test_that("dist contains NA | invalid dist", {
  dA = fastDist(A)
  dA[1] = NA
  test_cases = list(dA,1:100,"a", as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(expandDist(test_cases[[i]], A,B))
  }
})

test_that("expandDist retains row names", {
  set.seed(61)
  A = matrix(rnorm(100), nrow = 10)
  B = matrix(rnorm(100), nrow = 10)
  rownames(A) = paste0("A", 1:10)
  rownames(B) = paste0("B", 1:10)
  dA = fastDist(A)
  dAB = expandDist(dA, A,B)

  rN = c(rownames(A), rownames(B))
  expect_equal(attr(dAB, "Labels"), rN)


  A = matrix(rnorm(100), nrow = 10)
  B = matrix(rnorm(100), nrow = 10)
  dA = fastDist(A)
  dAB = expandDist(dA, A,B)

  rN = as.character(1:20)
  expect_equal(attr(dAB, "Labels"), rN)
})





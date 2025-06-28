# tests/testthat/test-fastDistAB.R
set.seed(4000)
x = matrix(rnorm(100), nrow = 10)
y = matrix(rnorm(100), nrow = 10)

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

test_that("fastDistAB gives similar results as proxy::dist", {
  methods = c("euclidean", "manhattan", "maximum", "minkowski", "minkowski")
  #canberra distance not included
  ps = c(rep(2,4),10,100)

  for(i in seq_along(methods)){
    fastDist_ = as.vector(fastDistAB(x, y, methods[i], p = ps[i]))
    proxydist_ = as.vector(proxy::dist(x, y, methods[i], p = ps[i]))
    expect_equal(fastDist_, proxydist_)
  }
})

test_that("fastDistAB gives correct results for canberra distance", {
  fastDist_ = as.vector(fastDistAB(x, y, "canberra"))
  canberra_dist_ = as.vector(canberra_dist(x,y))
  expect_equal(fastDist_, canberra_dist_)
})

test_that("fail if nrow(A)*nrow(B) > MAX.INT", {
  x_large = as.matrix(rnorm(10^6))
  y_large = as.matrix(rnorm(10^6))
  expect_error(fastDistAB(x_large, y_large))
})

test_that("fail if different ncol", {
  x_ = matrix(rnorm(100), nrow = 20)
  y_ = matrix(rnorm(100), nrow = 5)
  expect_error(fastDistAB(x_, y_))
})


test_that("invalid method", {

  test_cases = list(c("Sydney", "Melbourne", "Brisbane"),
                    T, F, NA, 22)

  for(i in seq_along(test_cases)){
    expect_error(fastDistAB(x,y, test_cases[[i]]))
  }

})

test_that("invalid p", {
  test_cases = list(NA, "a", NULL, T)

  for(i in seq_along(test_cases)){
    expect_error(fastDistAB(x, y, p = test_cases[[i]]))
  }

})


test_that("data contain NA | not numeric matrix", {

  test_cases = list(1:100, as.matrix(c(1:99, NA)))

  for(i in seq_along(test_cases)){
    expect_error(fastDistAB(test_cases[[i]],y))
  }
  expect_error(fastDistAB(x, y[,2:10]))

})


test_that("fastDistAB retains row names", {
  set.seed(61)
  A = matrix(rnorm(100), nrow = 10)
  B = matrix(rnorm(100), nrow = 10)
  rownames(A) = paste0("A", 1:10)
  rownames(B) = paste0("B", 1:10)
  dAB = fastDistAB(A,B)

  expect_equal(rownames(dAB), rownames(A))
  expect_equal(colnames(dAB), rownames(B))

  A = matrix(rnorm(100), nrow = 10)
  B = matrix(rnorm(100), nrow = 10)
  dAB = fastDistAB(A,B)

  expect_equal(rownames(dAB), as.character(1:10))
  expect_equal(colnames(dAB), as.character(1:10))
})









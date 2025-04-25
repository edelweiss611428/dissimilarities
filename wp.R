library("Rcpp")
library("RcppArmadillo")
library("devtools")
library("roxygen2")
library("microbenchmark")
library("proxy")

#
# usethis::use_package("RcppArmadillo", type = "linkingto")
# usethis::use_package("Rcpp", type = "linkingto")
# usethis::use_package("Rcpp", type = "import")
# renv::snapshot()
# renv::status()

devtools::document()
devtools::load_all()

set.seed(1)
X = matrix(rnorm(10000), nrow = 100)



microbenchmark(fastDist(X, method = "minkowski", p = 5),
               stats::dist(X, method = "minkowski", p = 5),
               proxy::dist(X, method = "minkowski", p = 5),
               is.na(X),
               times = 100)

X = matrix(rnorm(9), nrow = 3)

x1 = as.vector(fastDist(X, method = "cosine"))
x2 = as.vector(proxy::dist(X, method = "cosine"))

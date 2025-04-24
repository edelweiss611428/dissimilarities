library("Rcpp")
library("RcppArmadillo")
library("devtools")
library("roxygen2")
library("microbenchmark")

usethis::use_package("RcppArmadillo", type = "linkingto")
usethis::use_package("Rcpp", type = "linkingto")
usethis::use_package("Rcpp", type = "import")
renv::snapshot()
renv::status()

devtools::document()
devtools::load_all()

set.seed(1)
X = matrix(rnorm(100000), nrow = 10)

library("proxy")
library("microbenchmark")

dissimilarities::dist(X)


microbenchmark(dissimilarities::dist(X),
               stats::dist(X))


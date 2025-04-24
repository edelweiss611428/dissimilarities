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
x = c(rnorm(99), NA)
y = rnorm(10^2)

library("proxy")
library("microbenchmark")
dissimilarities::getdist(x,y)
proxy::dist(t(x),t(y))
mat = cbind(x,y)

microbenchmark(dissimilarities::getdist(x,y),
               proxy::dist(t(x),t(y)),
               stats::dist(t(mat)))

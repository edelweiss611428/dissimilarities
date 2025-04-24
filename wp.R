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
x = rnorm(10^2)
y = rnorm(10^2)

library("proxy")
library("microbenchmark")
dissimilarities::DistMaximum(x,y)
proxy::dist(t(x),t(y), method = "maximum")
mat = cbind(x,y)
stats::dist(t(mat), method = "maximum")

microbenchmark(dissimilarities::DistMinkowski(x,y, 2),
               proxy::dist(t(x),t(y)),
               stats::dist(t(mat), method = "canberra"))

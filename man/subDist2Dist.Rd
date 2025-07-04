% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subDist2Dist.R
\name{subDist2Dist}
\alias{subDist2Dist}
\title{Dist2Dist subsetting}
\usage{
subDist2Dist(dist, idx, diag = FALSE, upper = FALSE)
}
\arguments{
\item{dist}{A "dist" object, which can be computed via the stats::dist function, representing the full pairwise distance matrix between observations.}

\item{idx}{An integer vector, specifying the indices of the observations to retain.}

\item{diag}{A boolean value, indicating whether to display the diagonal entries.}

\item{upper}{A boolean value, indicating whether to display the upper triangular entries.}
}
\value{
A numeric "matrix" storing pairwise distances between the selected observations.
}
\description{
Efficiently extracts a subset of observations from a "dist" object and returns a new "dist" object representing only the selected distances.
}
\details{
This function subsets a "dist" object directly without explicit conversion to a dense distance "matrix".
It extracts only the relevant distances corresponding to the selected indices, improving both performance and memory efficiency.
The result is returned as a subsetted "dist" object, preserving compatibility with downstream functions that accept this class.

Row names are retained. If it is null, as.character(idx) will be used as row names instead.
}
\examples{

library("microbenchmark")
x = matrix(rnorm(200), nrow = 50)
dx = dist(x)
#Subsetting the first 10 units
microbenchmark(as.dist(base::as.matrix(dx)[1:10,1:10]),
               as.dist(proxy::as.matrix(dx)[1:10,1:10]),
               subDist2Dist(dx, 1:10))
#Check if equal
v1 = as.vector(as.dist(base::as.matrix(dx)[1:10,1:10]))
v2 = as.vector(subDist2Dist(dx, 1:10))
all.equal(v1, v2)

}
\author{
Minh Long Nguyen \email{edelweiss611428@gmail.com}
}

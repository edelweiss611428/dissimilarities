# Welcome to dissimilarities
[![R-CMD-check](https://github.com/edelweiss611428/dissimilarities/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edelweiss611428/dissimilarities/actions/workflows/R-CMD-check.yaml) [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/edelweiss611428/dissimilarities/graphs/commit-activity) [![dissimilarities status badge](https://edelweiss611428.r-universe.dev/dissimilarities/badges/version)](https://edelweiss611428.r-universe.dev/dissimilarities)
[![CRAN Version](https://www.r-pkg.org/badges/version/dissimilarities)](https://CRAN.R-project.org/package=dissimilarities) 
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/dissimilarities)](https://CRAN.R-project.org/package=dissimilarities) [![codecov](https://codecov.io/gh/edelweiss611428/dissimilarities/branch/main/graph/badge.svg)](https://codecov.io/gh/edelweiss611428/dissimilarities)

### Description

<p align="justify"> The package provides efficient and user-friendly functions for creating, manipulating, and subsetting "dist" objects, which are commonly used in clustering applications in R. </p> 

The current version includes the following features:    
     
- **Dist2Mat**: Converting a "dist" object to a numeric matrix.
- **subDist2Dist**: Subsetting a "dist" object to a  "dist" object.
- **subDist2Mat**: Subsetting a "dist" object to a numeric "matrix".
- **subCols**: Subsetting a "dist" object to a numeric "matrix" based on some column (equivalently, row) indices.
- **fastDist**: Computing a "dist" object given a numeric matrix.
- **fastDistAB**: Computing a numeric "matrix" storing pairwise distances between rows in two matrices.
- **get2dFrom1d**: Computing 2D-indexing given 1D-indexing (as used in R's "dist" objects).
- **get1dFrom2d**: Computing 1D-indexing given 2D-indexing (a row-column pair).
- **expandDist**: Expanding a "dist" object given new data.
  
Feature(s) under development:   
  
- Providing additional distance functions in fastDist and fastDistAB. The current version only supports popular metrics, namely "euclidean", "manhattan", "minkowski", "maximum", "canberra", and "cosine".

 ### Installation

 To download the newest version of the package, use the following R code: 

```r
library(devtools)
install_github("edelweiss611428/dissimilarities") 
```

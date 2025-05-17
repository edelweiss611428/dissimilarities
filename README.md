# dissimilarities

### Description

<p align="justify"> The package provides efficient and user-friendly functions for creating, manipulating, and subsetting "dist" objects, which are commonly used in clustering applications in R. </p> 
The current version includes the following features:

- Dist2Mat: Converting a "dist" object to a numeric matrix.
- subDist2Dist: Subsetting a "dist" object to a  "dist" object without explicit conversion to a numeric matrix.
- subDist2Mat: Subsetting a "dist" object to a numeric "matrix" without explicit conversion to a numeric matrix.
- subCols: Subsetting a "dist" object to a numeric "matrix" based on some column (equivalently, row) indices.
- fastDist: Computing a "dist" object given a numeric matrix.
- fastDistAB: Computing a numeric "matrix" storing pairwise distances between rows in two matrices.
- get2dFrom1d: Computing 2D-indexing given 1D-indexing (as used in R's "dist" objects).
- get1dFrom2d: Computing 1D-indexing given 2D-indexing (a row-column pair).
- expandDist: Expanding a "dist" object given new data.

Feature(s) under development:

- Providing additional distance functions in fastDist and fastDistAB. The current version only supports popular metrics, namely "euclidean", "manhattan", "minkowski", "maximum", "canberra", and "cosine".

 ### Installation

 To download the package, use the following R code: 

```
library(devtools)
install_github("edelweiss611428/dissimilarities") 
```

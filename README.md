# dissimilarities

### Description

<p align="justify"> The package provides efficient functions for creating, manipulating, and subsetting "dist" objects, which are commonly used in cluster analysis in R. </p> 
The current version includes the following features:

- Dist2Mat: Converting a "dist" object to a numeric matrix.
- subDist2Dist: Subsetting a "dist" object to a  "dist" object without explicitly converting it to a numeric matrix.
- subDist2Mat: Subsetting a "dist" object to a numeric "matrix" without explicitly converting the object to a numeric matrix.
- subCols: Subsettings a "dist" object to a numeric "matrix" based on some column (equivalently, row) indices.
- fastDist: Computing a "dist" object given a numeric matrix.
- fastDistAB: Computing a numeric "matrix" storing pairwise distances between rows in two matrices.

Features under development:

- Providing additional distance functions in fastDist and fastDistAB.
- expandDist: Expanding a "dist" object given new data.

 ### Installation

 To download the package, use the following R code: 

```
library(devtools)
install_github("edelweiss611428/dissimilarities") 
```

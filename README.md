# bnspatial
Function wrapper for the spatial implementation of Bayesian Networks in R and more

How to install (until the package is not available on CRAN): download the source file (bnspatial_1.0.tar.gz) and run the following command in R:
``` r
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
install.packages("gRain", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages("raster", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages("foreach", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages("doParallel", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages('bnspatial_1.0.tar.gz', repos = NULL, type="source")
```

# bnspatial
Function wrapper for the spatial implementation of Bayesian Networks in R and more.

To install (until the package is not available on CRAN):
- download the source file [bnspatial_0.9.tar.gz](https://github.com/dariomasante/bnspatial/blob/master/bnspatial_0.9.tar.gz?raw=true) to the R working directory (or any other directory)
- start an R session
- run the following commands in the console:
``` r
## Install the required packages 
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
install.packages("gRain", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages("raster", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)

## Install bnspatial (full path to the file, if not in the R working directory)
install.packages("~/bnspatial_0.9.tar.gz", repos = NULL, type="source")

## Load package
library(bnspatial)

?bnspatial
```

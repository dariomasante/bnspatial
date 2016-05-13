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

?bnSpatialize
```

## Overview
This package is aimed at implementing Bayesian networks in geographical space. It allows to obtain maps of expected value (or most likely state) given known and unknown conditions specified in the network, maps of uncertainty measured as coefficient of variation or Shannon index (entropy), maps of probability associated to any states from any node of the network. Utility nodes are currently under development.
The package is designed to facilitate the spatial implementation of Bayesian networks with minimal knowledge of the R programming language. The use of R provides a single consistent working environment, while currently available options to make maps out of Bayesian networks do not. The package acts partly as function wrapper integrating the packages [`raster`](https://cran.r-project.org/web/packages/raster/index.html) and [`gRain`](https://cran.r-project.org/web/packages/gRain/index.html), but offers some additional features too, including powerful parallel processing options (via [`foreach`](https://cran.r-project.org/web/packages/foreach/index.html) package) and data discretization routines.


Here it follows an example using function `bnSpatialize`, which wraps most bnspatial functions into a single step, taking from network and input data to the output maps. 
```{r, message=FALSE, warning=FALSE}
library(bnspatial)
data(ConwyData)
network <- LandUseChange
spatialData <- c(currentLU, slope, status)
lookup <- LUclasses

bn <- bnSpatialize(network, 'FinalLULC', spatialData, lookup)
bn
```

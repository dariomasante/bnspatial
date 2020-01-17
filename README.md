# bnspatial
Package for the spatial implementation of Bayesian Networks and mapping in geographical space. 
Outputs are GIS ready maps of expected value (or most likely state) given known and unknown conditions, maps of uncertainty measured as both coefficient of variation or Shannon index (entropy), maps of probability associated to any states of any node of the network. Some additional features are provided as well, such as parallel processing options, data discretization routines and function wrappers designed for users with minimal knowledge of the R programming language.

URL: https://cran.r-project.org/package=bnspatial

To install, open a R session and enter the following commands, which will install some mandatory functions underlying *gRain*:
```r
# For R version 3.6
if (!requireNamespace("BiocManager")) install.packages("BiocManager")
BiocManager::install('RBGL')

# Use this for R version 3.5
if (!requireNamespace("BiocManager")) install.packages("BiocManager")
BiocManager::install('RBGL', version = "3.8")

# ...or this for older versions:
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
```
Then install *bnspatial*, by selecting it from the packages list, or typing in the console:
```r
install.packages("bnspatial")
```

Alternatively, to install from source:
- download the [source file (.tar.gz)](https://cran.r-project.org/package=bnspatial) to the R working directory (or any other directory)
- start an R session
- run previous Biocmanager installations and then execute these commands:
``` r
install.packages("gRain", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
install.packages("raster", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)

## Install bnspatial (full path to the file, if not in the R working directory)
install.packages("~/bnspatial_[LATEST_VERSION].tar.gz", repos = NULL, type="source")
```

Reference manual: [bnspatial.pdf](https://cran.r-project.org/package=bnspatial/bnspatial.pdf)  

Vignette with guided example: [Overview of package 'bnspatial'](https://cran.r-project.org/web/packages/bnspatial/vignettes/bnspatial.html)  

Also from R console:
```r
help(package=bnspatial) ## opens package index
?bnspatial ## help file for the main function
```

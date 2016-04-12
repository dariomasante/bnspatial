installMissingPackages <- function(parallel=TRUE){
  if(parallel == TRUE){
    pack <- library(foreach, logical.return = TRUE)
    if(pack == FALSE){
      install.packages("foreach", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
      install.packages("doParallel", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
    }
    library(doParallel)
  }
  pack <- library(gRain, logical.return = TRUE)
  if(pack == FALSE){
    source("http://bioconductor.org/biocLite.R")
    biocLite("RBGL")
    install.packages("gRain", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
    library(gRain)
  }
  pack <- library(raster, logical.return = TRUE)
  if(pack == FALSE){
    install.packages("raster", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
    library(raster)
  }
}

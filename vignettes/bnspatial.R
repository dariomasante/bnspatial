## ---- eval=FALSE---------------------------------------------------------
#  # Use this for R version > 3.5.0
#  if (!requireNamespace("BiocManager")) install.packages("BiocManager")
#  BiocManager::install('RBGL', version = "3.8")
#  
#  # ...or this for older versions:
#  source("http://bioconductor.org/biocLite.R")
#  biocLite("RBGL")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("bnspatial")

## ---- eval=FALSE---------------------------------------------------------
#  ## Install the required packages
#  # Use this for R version > 3.5.0
#  if (!requireNamespace("BiocManager")) install.packages("BiocManager")
#  BiocManager::install('RBGL', version = "3.8")
#  
#  # ...or this for older versions:
#  source("http://bioconductor.org/biocLite.R")
#  biocLite("RBGL")
#  
#  # Then:
#  install.packages("gRain", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
#  install.packages("raster", repos="http://cran.uk.r-project.org/", dependencies=T, clean=T)
#  
#  ## Install bnspatial (full path to the file, if not in the R working directory)
#  install.packages("~/bnspatial_1.0.5.tar.gz", repos = NULL, type="source")

## ---- eval=FALSE---------------------------------------------------------
#  help(package=bnspatial) ## opens package index
#  ?bnspatial ## help file for the main function

## ---- message=FALSE, warning=FALSE, results='hide', fig.height=4, fig.width=6----
## Loading package and data stored in it
library(bnspatial)
data(ConwyData)
list2env(ConwyData, environment())

network <- LandUseChange
spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
lookup <- LUclasses
target <- 'FinalLULC'

## Run a spatial query on the Bayesian network 
## A summary will be printed on screen to check whether spatial data correspond 
## to network nodes as intended.
bn <- bnspatial(network, target, spatialData, lookup)
bn

## Plot output maps
library(raster)
par(mfrow=c(1,2))
plot(bn$Class, main='Most likely class')
plot(bn$Entropy, main='Uncertainty (Shannon index)')

## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------
bn <- bnspatial(network, 'FinalLULC', spatialData, lookup, verbose=FALSE)

## ---- message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  ## Loading data stored in package
#  network <- 'fullPath/toPackageLibrary/bnspatial/extdata/LandUseChange.net'
#  
#  spatialData <- c('fullPath/toPackageLibrary/bnspatial/extdata/CurrentLULC.tif',
#                   'fullPath/toPackageLibrary/bnspatial/extdata/degSlope.tif',
#                   'fullPath/toPackageLibrary/bnspatial/extdata/LegalStatus.tif')
#  
#  lookup <- 'fullPath/toPackageLibrary/bnspatial/extdata/LUclasses.txt'
#  
#  ## Run a spatial query on the Bayesian network
#  bn <- bnspatial(network, 'FinalLULC', spatialData, lookup)

## ---- message=FALSE, warning=FALSE, results='hide', fig.height=4, fig.width=6----
bn <- bnspatial(network, 'FinalLULC', spatialData, lookup, msk=ConwyLU, 
                   what="probability", targetState=c("arable","forest"))
par(mfrow=c(1,2))
plot(bn$Probability$forest, main="Probability of forest")
plot(bn$Probability$arable, main="Probability of arable land")

## ---- message=FALSE, warning=FALSE, results='hide', fig.height=8, fig.width=6----
par(mfrow=c(2,2))

bn <- bnspatial(network, "RelativePreference", spatialData, lookup, msk=ConwyLU, 
                   Scenarios="intensification")
plot(bn$Class, main="Intensification scenario")
plot(bn$Entropy, main="Uncertainty (Shannon index)")

bn <- bnspatial(network, 'RelativePreference', spatialData, lookup, msk=ConwyLU, 
                   Scenarios="sustainable")
plot(bn$Class, main="Sustainability scenario")
plot(bn$Entropy, main="Uncertainty (Shannon index)")

## ---- message=FALSE, warning=FALSE, results='hide', fig.height=8, fig.width=6, echo=-c(1:2)----
par(mfrow=c(2,2))

bn <- bnspatial(network, "RelativePreference", spatialData, lookup, msk=ConwyLU, 
                   what="probability", targetState=c("forest","arable"), Scenarios="intensification")
plot(bn$Probability$forest, main="P of forest preference (intensif.)")
plot(bn$Probability$arable, main="P of arable preference (intensif.)")

bn <- bnspatial(network, 'RelativePreference', spatialData, lookup, msk=ConwyLU, 
                   what="probability", targetState=c("forest","arable"), Scenarios="sustainable")
plot(bn$Probability$forest, main="P of forest preference (sustain.)")
plot(bn$Probability$arable, main="P of arable preference (sustain.)")

## ---- message=FALSE, warning=FALSE, results='hide', echo=-c(1:2), fig.height=8, fig.width=6----
par(mfrow=c(2,2))

midValues <- c(175, 75, 15)

bn <- bnspatial(network, "CarbonStock", spatialData, lookup, msk=ConwyLU, 
                   midvals=midValues, what=c("expected","variation"), Scenarios="intensification")
plot(bn$ExpectedValue, main="Expected carbon (t/ha) (intensif.)")
plot(bn$CoeffVariation, main="Uncertainty (coeff. variation)")

bn <- bnspatial(network, "CarbonStock", spatialData, lookup, msk=ConwyLU, 
                   midvals=midValues, what=c("expected","variation"), Scenarios="sustainable")
plot(bn$ExpectedValue, main="Expected carbon (t/ha) (sustain.)")
plot(bn$CoeffVariation, main="Uncertainty (coeff. variation)")

## ---- eval=FALSE---------------------------------------------------------
#  bn <- bnspatial(network, 'FinalLULC', spatialData, lookup, inparallel=2)

## ---- eval=FALSE---------------------------------------------------------
#  network <- loadNetwork(LandUseChange, target='FinalLULC')
#  network

## ---- eval=FALSE---------------------------------------------------------
#  fullpath <- system.file("extdata", "LUclasses.txt", package = "bnspatial")
#  fullpath
#  
#  intervals <- importClasses(fullpath)
#  intervals

## ---- eval=FALSE---------------------------------------------------------
#  spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#  spatialDataList <- linkMultiple(spatialData, network, intervals)

## ---- eval=FALSE---------------------------------------------------------
#  ## Return coordinates of valid cells inside the mask instead of a raster layer
#  msk <- aoi(spatialData, msk=ConwyLU, xy=TRUE)
#  head(msk)

## ---- eval=FALSE---------------------------------------------------------
#  m <- aoi(msk=ConwyLU, mskSub=c(2,3))
#  head( extractByMask(ConwySlope, msk=m), 20)

## ---- eval=FALSE---------------------------------------------------------
#  s <- runif(30)
#  
#  ## Split by user defined values. Values out of boundaries are set to NA:
#  dataDiscretize(s, classBoundaries = c(0.2, 0.5, 0.8))

## ---- eval=FALSE---------------------------------------------------------
#  q <- queryNet(network, 'FinalLULC', evidence)
#  head(q)

## ---- eval=FALSE---------------------------------------------------------
#  target <- 'FinalLULC'
#  statesProb <- queryNet(network, target, evidence)
#  maps <- mapTarget(target, statesProb, msk=ConwyLU)
#  maps
#  
#  par(mfrow=c(1,2))
#  plot(maps$Class, main='Most likely land use class')
#  plot(maps$Entropy, main='Uncertainty (Shannon index)')


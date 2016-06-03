## Build the .RData from raw

## Make an empty blank (vanilla) R session (or better restart the R session in vanilla mode)
rm(list=ls()) ## note this may not be enough

## Land use change example
library(raster)
library(bnspatial)

rawPath <- system.file("extdata/ConwyStatus.tif", package = "bnspatial")
ConwyStatus <- raster(rawPath)
ConwyStatus <- readAll(ConwyStatus)

rawPath <- system.file("extdata/ConwySlope.tif", package = "bnspatial")
ConwySlope <- raster(rawPath)
ConwySlope[ConwySlope == 128] <- NA

rawPath <- system.file("extdata/ConwyLU.tif", package = "bnspatial")
ConwyLU <- raster(rawPath)
ConwyLU <- readAll(ConwyLU)

rawPath <- system.file("extdata/LUclasses.txt", package = "bnspatial")
LUclasses <- importClasses(rawPath)

rawPath <- system.file("extdata/LandUseChange.net", package = "bnspatial")
LandUseChange <- loadNetwork(rawPath,'FinalLULC')

spDataLst <- linkMultiple(c(ConwyLU, ConwySlope, ConwyStatus), LandUseChange, LUclasses, verbose = FALSE)
coord <- aoi(ConwyLU, xy=TRUE)
evidence <- bulkDiscretize(spDataLst, coord)

rm(rawPath)
rm(coord)
rm(spDataLst)
rm(.Random.seed)
#save.image(file=system.file("data/ConwyData.RData", package = "bnspatial"))


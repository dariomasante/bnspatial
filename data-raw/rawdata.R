## Build the .RData from raw

## Start an empty blank (vanilla) session

## Land use change example
library(raster)
library(bnspatial)

rawPath <- system.file("extdata/LegalStatus.tif", package = "bnspatial")
status <- raster(rawPath)
status <- readAll(status)

rawPath <- system.file("extdata/degSlope.tif", package = "bnspatial")
slope <- raster(rawPath)
slope[slope == 128] <- NA

rawPath <- system.file("extdata/CurrentLULC.tif", package = "bnspatial")
currentLU <- raster(rawPath)
currentLU <- readAll(currentLU)

rawPath <- system.file("extdata/LUclasses.txt", package = "bnspatial")
LUclasses <- importClasses(rawPath)

rawPath <- system.file("extdata/LandUseChange.net", package = "bnspatial")
LandUseChange <- loadNetwork(rawPath,'FinalLULC')

spDataLst <- linkMultiple(c(currentLU, slope, status), LandUseChange, LUclasses, verbose = FALSE)
coord <- aoi(currentLU, xy=TRUE)
evidence <- bulkDiscretize(spDataLst, coord)

rm(rawPath)
rm(coord)
rm(spDataLst)
rm(.Random.seed)

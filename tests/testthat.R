library(testthat)
library(bnspatial)

raw = system.file("extdata", "LandUseChange.net", package = "bnspatial")
data(ConwyData)
list2env(ConwyData, environment())
network <- LandUseChange
lookup <- LUclasses
spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
Conwy = sf::st_read(system.file("extdata", "Conwy.shp", package = "bnspatial"))

test_check("bnspatial")

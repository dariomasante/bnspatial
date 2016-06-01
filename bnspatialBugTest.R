library(bnspatial)

##############################################################
## loadNetwork

## Good
raw = system.file("extdata", "LandUseChange.net", package = "bnspatial")
loadNetwork(raw)
loadNetwork(raw, 'FinalLULC')
data(bnspatial)
loadNetwork(LandUseChange)
loadNetwork(LandUseChange,'FinalLULC')

## Bad
loadNetwork(LandUseChange,'FinalLULC','FinalLULC')

##############################################################
## linkNodeRaster

## Good
linkNodeRaster(layer='N:/bnspatial_test/bnspatial/inst/extdata/CurrentLULC.tif', network=LandUseChange, node='CurrentLULC', intervals=c(2, 3, 1))
data(bnspatial)
linkNodeRaster(layer=currentLU, network=LandUseChange, node='CurrentLULC', intervals=c(2, 3, 1))
spatialData <- c(currentLU,slope,status)
spatialData <- c('N:/bnspatial_test/bnspatial/inst/extdata/CurrentLULC.tif','N:/bnspatial_test/bnspatial/inst/extdata/degSlope.tif','N:/bnspatial_test/bnspatial/inst/extdata/LegalStatus.tif')
lookup <- LUclasses
linkMultiple(spatialData, network, lookup, verbose = FALSE)

## Bad

## Fix



##############################################################
## dataDiscretize

s <- runif(100)

## Good
dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8))
dataDiscretize(s, classBoundaries=3)
dataDiscretize(s, classBoundaries=3, method = 'quantile')
dataDiscretize(s, classBoundaries=3, method = 'equal')
dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b', 'c'))
dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b', 'c'), method = 'equal')
dataDiscretize(s, classStates=c('a', 'b', 'c'))
dataDiscretize(s, classStates=c('a', 'b', 'c'), method = 'equal')
dataDiscretize(s, classStates=c('a', 'b'))
dataDiscretize(s, classStates=c(1, 2))
dataDiscretize(s, classBoundaries=c(-Inf,0.2,Inf)) 
dataDiscretize(s, classBoundaries=c(-Inf, 0.2, 0.5, 0.8))
dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8, Inf))

## Bad
dataDiscretize(s)
dataDiscretize(s, classBoundaries=3, method = 'quanle')
dataDiscretize(s, classBoundaries=c(0.5, 0.2)) 
dataDiscretize(s, classBoundaries=c(0.5, 0.8, 0.2)) 
dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2)) 
dataDiscretize(s, classBoundaries=0.2) 
dataDiscretize(s, classBoundaries=0) 
dataDiscretize(s, classBoundaries=1) 
dataDiscretize(s, classBoundaries=2.1) 
dataDiscretize(s, classBoundaries=2, classStates=c('a', 'b', 'c'))
dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b'))
dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2), classStates=c('a', 'b'))
dataDiscretize(s, classBoundaries='2')
dataDiscretize(s, classBoundaries=c(Inf,0.2,Inf))
dataDiscretize(s, classStates=3)
dataDiscretize(s, classStates='a')
dataDiscretize(s, classBoundaries=c(1, 2))

## Warning
dataDiscretize(s, classBoundaries=c(-Inf,1.2,Inf)) 
dataDiscretize(s, classBoundaries=c(-Inf,0.8,1.1,1.2,Inf))
dataDiscretize(s, classBoundaries=c(1.2, 2, 3.1))
dataDiscretize(s, classBoundaries=c(0.2, 0.5)) ## Borderline (should fail in a BN context)
dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8, 0.8, 1.0))
dataDiscretize(s, classBoundaries=c(0.5, 0.5))


## Fix

##############################################################
### Use testthat
library(testthat)
expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.2)) ) 
expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.8, 0.2)) )
expect_error(dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2)) )
expect_error(dataDiscretize(s, classBoundaries=0.2) )
expect_error(dataDiscretize(s, classBoundaries=0) )
expect_error(dataDiscretize(s, classBoundaries=1) )
expect_error(dataDiscretize(s, classBoundaries=2.1) )
expect_error(dataDiscretize(s, classBoundaries=3, method = 'quanle') )


##############################################################
## Good
setClass(c('Slope', 'CurrentLULC', 'LegalStatus'), list(c('flat', 'moderate', 'steep'),
c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))), w='N:/delTest.txt')


## Bad
setClass(c('CurrentLULC', 'LegalStatus'), list(c('flat', 'moderate', 'steep'),
c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))), w='N:/delTest.txt')
setClass(c('CurrentLULC', 'LegalStatus'), 
list(c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))), w='N:/delTest.txt')
setClass(c('CurrentLULC', 'LegalStatus'), 
list(c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(3, 1), (c(4, 3, 1))), wr='N:/delTest.txt')
setClass(c('CurrentLULC', 'LegalStatus'), 
list(c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(0.5, 4, 3, 1), (c(4, 3, 1))), wr='N:/delTest.txt')


## Fix
setClass(c('CurrentLULC', 'LegalStatus'), list(c('flat', 'moderate', 'steep'),
c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))), w='N:/delTest.txt')











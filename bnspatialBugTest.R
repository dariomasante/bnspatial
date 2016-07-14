library(bnspatial)
library(testthat)
##################### GENERAL features #######################
# Make PDF vignettes
# Add example in risk analysis
# Swap current example

##############################################################
## extractByMask
# % FIX CHECK FOR MISSING NA
 
##############################################################
## queryNet
# % FIX Harmonize with .freezeEvidence the first bit of queryNet, lots of redundancy
# % FIX Accept non factors in input evidence table
# % FIX Accept data.frame as input evidence table, not just matrices
# % FIX Ignore extra columns when in evidence tab, instead of throwing error

##############################################################
## mapTarget
#' % FIX COORD.REF. OUTPUT RASTERS
#' % FIX OUTPUT VALUE OF MOST LIKELY CLASS (to correspod if integer are provided)
#' % ADD UTILITY VALUE
#' % ADD AN EXAMPLE USING MIDVALUES
#' % ADD HUGE RASTER MANAGEMENT
# % ADD Output algoritms now hidden (e.g. expected value) to exportable functions

## Good


## Bad

##############################################################
## loadNetwork
# % FIX UNDERSCORES REMOVAL (gRain:::.getNodeSpec AND gRain:::.toCamel)
# % CHANGE rewrite all external file reading, remove dependency from gRain there
# % ADD read genie native formats directly xml files
# % ADD read utility nodes


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
## linkNode
# % NOTE: FIX ORDER OF NODE STATES BETWEEN CLASSIFICATION AND SPATIAL DATA

## TO FIX

## Good

## Bad


##############################################################
## linkNodeRaster

## TO FIX

## Good
linkNodeRaster(layer='N:/bnspatial/inst/extdata/CurrentLULC.tif', network=LandUseChange, node='CurrentLULC', intervals=c(2, 3, 1))
data(bnspatial)
linkNodeRaster(layer=ConwyLU, network=LandUseChange, node='CurrentLULC', intervals=c(2, 3, 1))
spatialData <- c(ConwyLU,ConwySlope,ConwyStatus)
spatialData <- c('N:/bnspatial/inst/extdata/CurrentLULC.tif','N:/bnspatial/inst/extdata/degSlope.tif','N:/bnspatial/inst/extdata/LegalStatus.tif')
lookup <- LUclasses
linkMultiple(spatialData, network, lookup, verbose = FALSE)

## Bad


##############################################################
## dataDiscretize

s <- runif(100)

## TO FIX


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



##############################################################
### Use testthat

expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.2)) ) 
expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.8, 0.2)) )
expect_error(dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2)) )
expect_error(dataDiscretize(s, classBoundaries=0.2) )
expect_error(dataDiscretize(s, classBoundaries=0) )
expect_error(dataDiscretize(s, classBoundaries=1) )
expect_error(dataDiscretize(s, classBoundaries=2.1) )
expect_error(dataDiscretize(s, classBoundaries=3, method = 'quanle') )


##############################################################
## setClasses

## TO FIX
setClass(c('CurrentLULC', 'LegalStatus'), list(c('flat', 'moderate', 'steep'),
        c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
         list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))), w='N:/delTest.txt')

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

########## BIGGER DATA (below data evidence is provided for only ten nodes, as query 
########## time start growing exponentially afterwards)
library(raster); library(bnspatial)

net = loadNetwork('N:/A.net')
net = loadNetwork('N:/Hepar.net')

r = raster('N:/bnspatial/inst/extdata/CurrentLULC.tif')

set.seed(99)
spNodes = net$universe$levels[sample(1:70, 10)]
ls = list()
for(n in seq_along(spNodes)){
    rr = r
    nNA = which(!is.na(getValues(rr)))
    rr[nNA] = sample(length(spNodes[[n]]), length(nNA), replace=TRUE)
    assign(names(spNodes[n]), rr)
    ls = c(ls, get(names(spNodes[n])))
    cat(
        paste(names(spNodes[n]), '\n', 
              paste(spNodes[[n]], collapse=','), '\n', 
              paste(sample(length(spNodes[[n]])), collapse=','), '\n', sep='')
        , file="N:/classes.txt", append=TRUE)
}

bnspatial(net, 'Cirrhosis', ls, 'N:/classes.txt', inparallel=TRUE)

file.remove("N:/classes.txt")



library(bnspatial)


###### aoi
## Make a mask from a group of input layers:
data(ConwyData)
network <- LandUseChange
spatialData <- c(currentLU, slope, status)
spDataLst <- linkMultiple(spatialData, network, LUclasses, verbose = FALSE)
m <- aoi(spDataLst)
m

## Plot mask
m <- aoi(currentLU)
plot(m)

## Make mask from a subset of values and plot
m <- aoi(currentLU, mskSub=c(2,3))
plot(m)

## Return coordinates of valid mask locations
coord <- aoi(currentLU, xy=TRUE)
head(coord)


###### bnSpatialize
data(ConwyData)
network <- LandUseChange
spatialData <- c(currentLU, slope, status)
lookup <- LUclasses

bn <- bnSpatialize(network, 'FinalLULC', spatialData, lookup)
bn

######
data(ConwyData)
network <- LandUseChange
target <- 'FinalLULC'
statesProb <- queryNet(network, target, evidence)

maps <- mapTarget(target, statesProb, msk=currentLU)
plot(maps$Class)
plot(maps$Entropy)

## Create a probability surface for the "forest" state of target node "FinalLULC"
mp <- mapTarget('FinalLULC', statesProb, what='probability', targetState='forest', msk=currentLU)
plot(mp$Probability$forest)

###### dataDiscretize
s <- runif(30)

# Split by user defined values. Values out of boundaries are set to NA:
dataDiscretize(s, classBoundaries = c(0.2, 0.5, 0.8)) 

# Split by quantiles (default):
dataDiscretize(s, classStates = c('a', 'b', 'c'))

# Split by equal intervals:
dataDiscretize(s, classStates = c('a', 'b', 'c'), method = "equal")

# When -Inf and Inf are provided as external boundaries, $midValues of outer classes
# are calculated on the minimum and maximum values:
dataDiscretize(s, classBoundaries=c(0, 0.5, 1), classStates=c("first", "second"))[c(2,3)]
dataDiscretize(s, classBoundaries=c(-Inf, 0.5, Inf), classStates=c("first", "second"))[c(2,3)]

###### bulkDiscretize
## Discretize multiple spatial data by location
data(ConwyData)
network <- LandUseChange
spatialData <- c(currentLU, slope, status)

# Link spatial data to the network nodes and discretize
spDataLst <- linkMultiple(spatialData, network, LUclasses, verbose = FALSE)
coord <- aoi(currentLU, xy=TRUE)
head( bulkDiscretize(spDataLst, coord) )

###### extractByMask
data(ConwyData)
m <- aoi(msk=currentLU, mskSub=c(2,3))
head( extractByMask(slope, msk=m), 20) 

# Extract making a raster
library(raster)
plot( extractByMask(slope, msk=m, spatial=TRUE) )

###### queryNet
data(ConwyData)

network <- LandUseChange
q <- queryNet(network, 'FinalLULC', evidence)
head(q)

# Fix a given node on a state (i.e. fixed evidence) by providing an additional argument
network <- LandUseChange
q <- queryNet(network, 'FinalLULC', evidence, Stakeholders = 'farmers')
head(q)

###### bnSpatialize
data(ConwyData)
network <- LandUseChange
spatialData <- c(currentLU, slope, status)
bn = bnSpatialize(network, 'FinalLULC', spatialData, LUclasses, msk=currentLU, spatial=TRUE, inparallel=FALSE, exportRaster=FALSE)
bn = bnSpatialize(network, 'FinalLULC', spatialData, LUclasses, msk=currentLU, spatial=TRUE, LegalStatus='private')
bn = bnSpatialize(network, 'FinalLULC', spatialData, LUclasses, spatial=TRUE, LegalStatus='private')
bn <- bnSpatialize(network, 'FinalLULC', spatialData, lookup, what='probability', targetState='forest')

###### .fixedEvidence
m = matrix(1:20, ncol=2, nrow=10);  colnames(m)=c('a','b')
df = data.frame(a=1:10,b=11:20)
nodes = c('w','b','y','a')
.fixedEvidence(m, nodes, b=1:10,a=99, p=msk)
.fixedEvidence(m, nodes, b=1:10,a=99)
.fixedEvidence(m, nodes, b=1:10)
.fixedEvidence(m, nodes, b=4)
.fixedEvidence(m, nodes, b=4, a=99)
.fixedEvidence(m, nodes, w=4)
.fixedEvidence(m, nodes, y=31:40, w=4)
.fixedEvidence(m, nodes)
.fixedEvidence(m, nodes, y=31:40, w=4, a=1)

.fixedEvidence(d, nodes, b=1:10,a=99, p=msk)
.fixedEvidence(d, nodes, b=1:10,a=99)
.fixedEvidence(d, nodes, b=4, a=99)
.fixedEvidence(d, nodes, b=1:10)
.fixedEvidence(d, nodes, b=4)
.fixedEvidence(d, nodes)
.fixedEvidence(d, nodes, w=4)
.fixedEvidence(d, nodes, y=31:40, w=4, a=10:1)


###################################################################
source('P:/EU ROBIN/Data/AgricYield/Models_Code/bnspatial_source.R')

setwd('N:\\bnspatial_test\\data\\')
network='LandUseChange.net'
target='FinalLULC'
spatialData = c('ConwyLULC.tif','ConwySlope.tif','LegalStatus.tif')
lookup = 'ConwyClasses.txt'
msk='ConwyLULC.tif'
###
installMissingPackages(multicores=FALSE)
###
loadNetwork('LandUseChange.net', 'FinalLULC')
###
setClasses(c('Slope', 'Current_LULC', 'LegalStatus'), 
	list(c('flat', 'moderate', 'steep'), c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
	list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1)))
)
###
importClasses('ConwyClasses.txt')
###
net = loadNetwork('LandUseChange.net')
linkNode('ConwyLULC.tif', net, 'CurrentLULC', c(2, 3, 1), categorical=NULL, verbose=TRUE)
###
net <- loadNetwork('LandUseChange.net')
spatialData <- c('ConwyLULC.tif','ConwySlope.tif','LegalStatus.tif')
lookup <- importClasses('ConwyClasses.txt')
linkMultiple(spatialData, net, lookup, verbose = FALSE)
###
net <- loadNetwork('LandUseChange.net')
spatialData <- c('ConwyLULC.tif','ConwySlope.tif','LegalStatus.tif')
lookup <- importClasses('ConwyClasses.txt')
spDataLst <- linkMultiple(spatialData, net, lookup, verbose = FALSE)
aoi(spDataLst)
plot(aoi(msk='ConwyLULC.tif'))
mask <- raster('ConwyLULC.tif')
plot(aoi(msk=mask, mskSubset=c(2,3)))
###
mask <- aoi(msk='ConwyLULC.tif', mskSubset=c(2,3))
r <- raster('ConwySlope.tif')
mask <- raster('ConwyLULC.tif')
head( extractValuesByMask(r, mask) )
###
bn = bnSpatialize(network, target, spatialData, lookup, msk, spatial=TRUE, mcores=FALSE, exportRaster=TRUE)
bn = bnSpatialize(network, target, spatialData, lookup, msk, spatial=TRUE, LegalStatus='private')


#########
installMissingPackages(multicores=TRUE)
target = 'Yield'
bn = loadNetwork('MaizeModel_v3_bio12.net',target)

## Load table with class boundaries, if available (otherwise make a list with node name and associated vector of class boundaries)
classes = importClasses('MaizeModel_v3_bio12_Classes.txt')

spatialData = c('P:/EU ROBIN/Data/AgricYield/InputData/Temperature.tif', 
'P:/EU ROBIN/Data/AgricYield/InputData/bio12.tif', 
'P:/EU ROBIN/Data/AgricYield/InputData/Nitrogen.tif', 
'P:/EU ROBIN/Data/AgricYield/InputData/Irrigation.tif', 
'P:/EU ROBIN/Data/AgricYield/InputData/SoilOrganicCarbon.tif',  
'P:/EU ROBIN/Data/AgricYield/InputData/MaizeYieldGap.tif', 
'P:/EU ROBIN/Data/AgricYield/InputData/MedianSlope.tif')

## Load or create mask
msk = aoi(spatialData, msk = 'ROBIN_countries.tif') 

## Load input spatial data and corresponding nodes and states into a list
spatialDataList = linkMultiple(spatialData,bn,classes)

## Extract midvalues for target node
breaks = c(0,1.2, 2.4, 3.8, 5.0, 6.2, 7.4, 10.0)
midValues = sapply(1:(length(breaks)-1), function(x) {(breaks[x] + breaks[x+1])/2})

## Identify, index and get coordinates of valid cells (= non NA) from area of interest/mask
id = msk
id[] = 1:length(id)
id = as.vector(id[!is.na(as.vector(msk))])
xy = xyFromCell(msk,id)

## Extract data from locations, discretize and query Bayesian network

	cl = makePSOCKcluster(detectCores())
	registerDoParallel(cl)
#registerDoSNOW(cl)
	tab = bulkDiscretize(spatialDataList, xy)
	probs = bulkSpatialQuery(bn, target, tab)
	stopCluster(cl); gc()

##############

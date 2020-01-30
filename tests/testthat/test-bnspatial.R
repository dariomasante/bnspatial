context("bnspatial raster")

# GOOD
bnspatial(network, 'FinalLULC', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=ConwySlope)
expect_is(head(bnspatial(network, 'FinalLULC', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=ConwySlope, spatial=FALSE) ), 'data.frame')
expect_is(head(bnspatial(network, 'FinalLULC', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=ConwySlope, spatial=FALSE, inparallel=TRUE) ), 'data.frame')
bnspatial(network, 'FinalLULC', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=ConwySlope, inparallel=TRUE)
bnspatial(network, 'FinalLULC', spatialData, lookup, what=c('probability','entropy','variation','class','expected'), msk=list(ConwySlope,ConwyLU), inparallel=TRUE)
bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU))
bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=list(ConwySlope,ConwyLU))
bnspatial(network, 'Scenarios', spatialData, lookup, what=c('probability','entropy','class','expected'), msk=list(ConwySlope,ConwyLU))
bnspatial(network, 'Scenarios', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU))
bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU))
bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU), midvals = c(0,1,4))
bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU), midvals = c(0,1,4), Scenarios='intensification')
#

# BAD
expect_error( bnspatial(network,  target=NA, spatialData, lookup, what='probability', msk=ConwySlope) )
expect_error( bnspatial(network, 'CarbonStock', spatialData, lookup, what=c('probability','variation','entropy','class','expected'), msk=list(ConwySlope,ConwyLU), midvals = c(0,1,4,20)) )


context("bnspatial vector")

bnspatial(network, 'FinalLULC', Conwy, lookup, what=c('class','entropy','probability'), field=c('LU','Slope','Status'))

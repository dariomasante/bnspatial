context('loadNetwork')
# % FIX UNDERSCORES REMOVAL (gRain:::.getNodeSpec AND gRain:::.toCamel)
# % CHANGE rewrite all external file reading, remove dependency from gRain there
# % ADD read utility nodes

## Good
expect_is(loadNetwork(raw), 'grain')
expect_is(loadNetwork(raw, 'FinalLULC'), 'grain')
expect_is(loadNetwork(LandUseChange), 'grain')
expect_is(loadNetwork(LandUseChange,'FinalLULC'), 'grain')
expect_is(loadNetwork('./inst/extdata/LandUseChange.xdsl'), 'grain')
expect_is(loadNetwork('./inst/extdata/LandUseChange.dne'), 'grain')

## Bad
expect_error(loadNetwork(LandUseChange,'FinalLULC','FinalLULC'))
expect_error(loadNetwork(LandUseChange,'FinalLULC','FinalLULC'))
expect_error(loadNetwork('FinalLULC',LandUseChange))
expect_error(loadNetwork(system.file("extdata", "LandUseChange.format", package = "bnspatial"), 'FinalLULC'))
expect_error(loadNetwork(LandUseChange,'Final'))
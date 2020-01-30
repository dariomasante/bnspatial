context('dataDiscretize')

s <- runif(100)

## TO FIX

## Good
# dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8))
# dataDiscretize(s, classBoundaries=3)
# dataDiscretize(s, classBoundaries=3, method = 'quantile')
# dataDiscretize(s, classBoundaries=3, method = 'equal')
# dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b', 'c'))
# dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b', 'c'), method = 'equal')
# dataDiscretize(s, classStates=c('a', 'b', 'c'))
# dataDiscretize(s, classStates=c('a', 'b', 'c'), method = 'equal')
# dataDiscretize(s, classStates=c('a', 'b'))
# dataDiscretize(s, classStates=c(1, 2))
# dataDiscretize(s, classBoundaries=c(-Inf,0.2,Inf)) 
# dataDiscretize(s, classBoundaries=c(-Inf, 0.2, 0.5, 0.8))
# dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8, Inf))

## Bad
expect_error(dataDiscretize(s), 'Must provide either "classBoundaries", "classStates" or both')
expect_error(dataDiscretize(s, classBoundaries=3, method = 'quanle'), "'arg' should be one of “quantile”, “equal”")
expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.2)) , '"classBoundaries" must be provided from lowest to highest')
expect_error(dataDiscretize(s, classBoundaries=c(0.5, 0.8, 0.2)) , '"classBoundaries" must be provided from lowest to highest')
expect_error(dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2)) , '"classBoundaries" must be provided from lowest to highest')
expect_error(dataDiscretize(s, classBoundaries=0.2), '"classBoundaries" must be an integer greater than 1, or a vector of values to be used as class boundaries')
expect_error(dataDiscretize(s, classBoundaries=0) , '"classBoundaries" must be an integer greater than 1, or a vector of values to be used as class boundaries')
expect_error(dataDiscretize(s, classBoundaries=1) , '"classBoundaries" must be an integer greater than 1, or a vector of values to be used as class boundaries')
expect_error(dataDiscretize(s, classBoundaries=2.1), '"classBoundaries" must be an integer greater than 1, or a vector of values to be used as class boundaries' )
expect_error(dataDiscretize(s, classBoundaries=2, classStates=c('a', 'b', 'c')), 'Number of bins must match the number of states')
expect_error(dataDiscretize(s, classBoundaries=3, classStates=c('a', 'b')), 'Number of bins must match the number of states')
expect_error(dataDiscretize(s, classBoundaries=c(0.8, 0.5, 0.2), classStates=c('a', 'b')), '"classBoundaries" must be provided from lowest to highest')
expect_error(dataDiscretize(s, classBoundaries='2') )
expect_error(dataDiscretize(s, classBoundaries=c(Inf,0.2,Inf)), '"classBoundaries" must be provided from lowest to highest')
expect_error(dataDiscretize(s, classStates=3), '"classStates" must be a vector of length greater than 1')
expect_error(dataDiscretize(s, classStates='a'), '"classStates" must be a vector of length greater than 1')
expect_error(dataDiscretize(s, classBoundaries=c(1, 2)), 'All classes (i.e. node states) fall out of input data range. Check "classBoundaries" or the input data.')

## Warning
expect_warning(dataDiscretize(s, classBoundaries=c(-Inf,1.2,Inf)) )
expect_warning(dataDiscretize(s, classBoundaries=c(-Inf,0.8,1.1,1.2,Inf)) )
expect_warning(dataDiscretize(s, classBoundaries=c(1.2, 2, 3.1)) )
expect_warning(dataDiscretize(s, classBoundaries=c(0.2, 0.5)) ) ## Borderline (should fail in a BN context)
expect_warning(dataDiscretize(s, classBoundaries=c(0.2, 0.5, 0.8, 0.8, 1.0)) )
expect_warning(dataDiscretize(s, classBoundaries=c(0.5, 0.5)) )

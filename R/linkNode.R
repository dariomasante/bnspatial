#' @name linkNode
#' @title Link nodes to spatial data
#' 
#' @description \code{linkNode} links a node of the Bayesian network to its corresponding spatial data, returning a list of objects, including the spatial data and relevant information about the node.\cr
#' \code{linkMultiple} operates on multiple spatial layers and nodes.
#' @aliases linkMultiple
#' @param layer	character (path to spatial data file) or an object of class "RasterLayer" or "SpatialPolygonsDataFrame". The spatial data corresponding to the network node in argument \code{node}.
#' @inheritParams loadNetwork
#' @param node character. A network node associated to the file in \code{layer} argument
#' @param intervals A list of numeric vectors. For categorical variables the spatial data values associated to each state of the node, for continuous variables the boundary values dividing into the corresponding states.
#' @param categorical logical. Is the node a categorical variable? Default is NULL.
#' @param field character. Only for vectorial data, the field/column name in the attribute table corresponding to the node.
#' @param verbose logical. If \code{verbose = TRUE} a summary of class boundaries and associated nodes and data will be printed to screen for quick checks.
#' @param spatialData character, or list of objects of class 'RasterLayer' or 'SpatialPolygonsDataFrame'. The spatial data associated to some network node, provided as file paths or as list of spatial objects of said classes. Must be ordered accordingly to the corresponding nodes in \code{lookup}, or provided as named list, where names correspond exactly to the corresponding node names. In case it is not a named list, but \code{lookup} contains already the optional 'layer' item, the latter will be passed to the loader function for each node.
#' @param lookup character or a formatted list. This argument can be provided as path to a comma separated file or a formatted list (see \code{\link{setClasses}} )
#' @return \code{linkNode} returns a list of objects, including the spatial data and summary information about each node.\cr
#' \code{linkMultiple} returns a list of lists. Each element of the list includes the spatial data and summary information for each of the input nodes.
#' @details In future releases, this function may be rewritten to provide an S4/S3 object.
#' @seealso \code{\link{dataDiscretize}}; \code{\link{setClasses}}
#' @examples
#' ## Load data into global environment
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' lst <- linkNode(layer=ConwyLU, network, node='CurrentLULC', intervals=c(2, 3, 1))
#' lst
#'
#' ## Link the Bayesian network to multiple spatial data at once, using a lookup list
#' spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#' lookup <- LUclasses
#' linkMultiple(spatialData, network, lookup, verbose = FALSE)
#' @export
linkNode <- function(layer, network, node, intervals, categorical=NULL, field=NULL, verbose=TRUE){
    network = loadNetwork(network=network)
    
    ## Perform checks
    # Check correspondence of node and states names between lookup list and network
    .checkNames(network, node)
    states <- network$universe$levels[[node]]
    # Check correspondence of number of states and intervals
    if(!identical(intervals, unique(intervals))){
        stop('Non unique intervals defined')
    }
    delta <- length(intervals) - length(states)
    if(!delta %in% c(0, 1)){
        stop('Number of classes does not match number of states.')
    }
    if(!is.null(categorical)){
        if(categorical == TRUE & delta != 0) {
            stop('Number of classes does not match number of states.')
        } else if(categorical == FALSE & delta != 1) {
            stop('Number of intervals does not match number of states. For non categorical data', 
                 'outer boundaries (minimum and maximum) must be set (-Inf and Inf allowed).')
        }
    }
    categorical <- ifelse(delta != 0, FALSE, TRUE)
    if( identical(intervals, sort(intervals)) == FALSE & categorical == FALSE){
        stop('"intervals" must be sorted from lowest to highest.')
    }

    ##
    layer <- .makeSpatial(layer, field)
    if(categorical == TRUE){
        if(class(layer) == 'RasterLayer'){
            v <- as.factor(raster::getValues(layer))
        } else {
            v <- as.factor(layer@data[ ,field])
        }
        if(is.null(intervals)){
            intervals <- as.numeric(levels(v))
            warning('For categorical data check classes integer value and corresponding states. If not matching,',
                    ' a look up list should be provided (function "setClasses") or modified from current list.')
        } else {
            ## May set as 
            if(identical(as.numeric(levels(v)), sort(intervals)) == FALSE){
                stop('Integer values in categorical data do not match categories provided.')
            }
        }
    }
    lst <- list(list(States = states, Categorical = categorical, ClassBoundaries = intervals, SpatialData = layer)) #FilePath = layer@file@name, 
    names(lst) <- node
    if(verbose == TRUE){
        writeLines(c(paste('\n"', node, '"', ' points to:', sep=''), 
                     paste(' -> ', layer@data@names, '\n'), 
                     'With states:', 
                     paste(states, collapse='    '), 
                     ifelse(is.null(categorical), '', ifelse(categorical == TRUE, 
                                                             '\nRepresented by integer values:', 
                                                             '\nDiscretized by intervals:')), 
                     paste(intervals, collapse= ' <-> '))
        )
        writeLines('----------------------------------')
    }
    return(lst)
}

#' @rdname linkNode
#' @export
linkMultiple <- function(spatialData, network, lookup, field=NULL, verbose=TRUE){
    if(is.character(lookup) & length(lookup) == 1){
        lookup <- importClasses(lookup)
    }
    if(length(spatialData) != length(lookup)){
        stop('Spatial data do not match the number of nodes provided in the look up list')
    }
    network = loadNetwork(network=network)
    # Check correspondence of node and states names between lookup list and network
    # then iterate through the nodes and append to summary list
    lst <- list()
    for(nm in names(lookup)){
        .checkStates(lookup[[nm]]$States, network$universe$levels[[nm]], nm)
        Categorical <- lookup[[nm]]$Categorical
        if(Categorical == TRUE){
            sortedClasses <- match(network$universe$levels[[nm]],lookup[[nm]]$States)
            ClassBoundaries <- lookup[[nm]]$ClassBoundaries[sortedClasses]
        } else {
            ClassBoundaries <- lookup[[nm]]$ClassBoundaries
        }
        if(is.null(names(spatialData)) ){ # names from this list get priority
            if(is.null(lookup[[nm]]$layer) ){
                n <- which(names(lookup) == nm) 
                spd <- spatialData[[n]] # associate by simple order of list
            } else {
                spd <- lookup[[nm]]$layer
            }
        } else {
            n <- which(names(spatialData) == nm)
            if(length(n) == 0){
                stop('No names in list "spatialData" matching the network node ', nm, 
                     ' :  \n', paste(names(spatialData), collapse=' '))
            }
            spd <- spatialData[[n]] 
        }
        lst[nm] <- linkNode(spd, network=network, node=nm, 
                            intervals=ClassBoundaries, categorical=Categorical, 
                            field=field[n], verbose=verbose)
    }
    return(lst)
}
####
.checkNames <- function(network, nodes){
    for(node in nodes){
        check <- node %in% network$universe$nodes
        if(check == FALSE){
            stop('"', node, '"', ' does not match any node names from the Bayesian network.')		
        }
    }
}
####
.checkStates <- function(inputStates, nodeStateNames, node=NULL){
    check <- inputStates %in% nodeStateNames
    msg <- ifelse(is.null(node), '.', paste(':', '"', node, '"'))
    if(any(check == FALSE)){
        stop(paste('Names of states provided do not match the node states from the network: \n"', 
                   inputStates[!check], '" missing from network node', msg))
    }
}
####
.makeSpatial <- function(item, fld=NULL, ...){ # TODO This should avoid loading the shape if field is missing
    if(is.character(item)){ 
        item <- tryCatch(raster::raster(item), error=function(e){
            sf::st_read(item, quiet = TRUE)
            cat('Trying as vectorial...')
        } )
    }
    if(!'RasterLayer' %in% class(item) & is.null(fld)){
        stop('"field" argument missing for ',item,'. Using vectorial data (e.g. shapefiles) ',
        'one field/column from the attribute table must be specified for each corresponding node.')
    }
    if('SpatialPolygonsDataFrame' %in% class(item)){
        item <- sf::st_as_sf(item) # transform from sp to sf
    }
    return(item) 
}
####
.checkFields <- function(shp, flds){
    shpInfo <- rgdal::ogrInfo(dirname(shp), gsub('.shp', '', basename(shp)))
    if(!all(fields %in% shpInfoa$iteminfo$name)){
        f <- flds[!flds %in% shpInfo$iteminfo$name]
        stop(paste(f, collapse=','),' missing from attribute table of ', shp)
    }
}

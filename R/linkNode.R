#' @name linkNode
#' @title Link nodes to spatial data
#' 
#' @description \code{linkNode} links a node of the Bayesian network to its corresponding spatial data, returning a list of objects, including the spatial data and relevant information about the node.\cr
#' \code{linkMultiple} operates on multiple spatial layers and nodes.
#' @aliases linkMultiple
#' @param layer	character (path to spatial data file) or an object of class "RasterLayer", "sf" or "SpatialPolygonsDataFrame". The spatial data corresponding to the network node in argument \code{node}.
#' @inheritParams loadNetwork
#' @param node character. A network node associated to the file in \code{layer} argument
#' @param intervals A list of numeric vectors. For categorical variables the spatial data values associated to each state of the node, for continuous variables the boundary values dividing into the corresponding states.
#' @param categorical logical. Is the node a categorical variable? Default is NULL.
#' @param field character. Only for vectorial data, the field/column name in the attribute table corresponding to the node, ordered accordingly.
#' @param verbose logical. If \code{verbose = TRUE} a summary of class boundaries and associated nodes and data will be printed to screen for quick checks.
#' @param spatialData character, or list of objects of class 'RasterLayer', or a single object of class 'sf' or 'SpatialPolygonsDataFrame'. The spatial data associated to some network node, provided as file paths or as (list of) spatial object of said classes. Must be ordered accordingly to the corresponding nodes in \code{lookup}, or provided as named list, where names correspond exactly to the corresponding node names. In case it is not a named list, but \code{lookup} contains already the optional 'layer' item, the latter will be passed to the loader function for each node.
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
#' 
#' ## Method for class 'sf' or 'SpatialPolygon' etc.
#' linkMultiple(spatialData, network, lookup, field= c('LU', 'Slope', 'Status'), verbose = FALSE)
#' 
#' @export
linkNode <- function(layer, network, node, intervals, categorical=NULL, field=NULL, verbose=TRUE){
    network = loadNetwork(network=network)
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
    layer <- .loadSpatial(layer, field)
    if(categorical == TRUE) {
        if('RasterLayer' %in% class(layer)){
            nm <- names(layer)
            uni <- raster::unique(layer)
            if(!all(uni %in% intervals) ) {
                vals = uni[!uni %in% intervals]
                warning('Some values in the spatial data do not have an associated state in the network node.',
                        'The following values will be masked out: ', paste(vals,collapse=', '))
                rcl <- cbind(from=vals, to=NA)
                layer <- raster::reclassify(layer, rcl)
                uni <- raster::unique(layer)
            }
            if(raster::is.factor(layer)) { # Check values correspond when raster table available
                df <- raster::levels(layer)[[1]]
                m <- data.frame(intervals, states, stringsAsFactors = FALSE)
                mm <- merge(df, m, by.x='ID', by.y='intervals', sort=FALSE)
                a <- !apply(mm, 1, function(x){x[2] == x[3] }) # check unmatching
                vals <- mm[a, ]
                if(!all(a)){
                    stop('Some values in categorical spatial data and associated states do not match: ',
                         'value ', paste(vals[,1], collapse=', '), ' corresponds to state "', paste(vals[,2], collapse=', '), 
                         '" in network node "', node, '", but to "', paste(vals[,3], collapse=', '), 
                         '" in associated spatial data "', nm,'"')
                }
            } else {
                layer <- raster::ratify(layer)
                df <- data.frame(ID=raster::levels(layer)[[1]])
                df$VALUE <- states[match(df$ID, intervals)]
                levels(layer) <- df
            }
        } else {
            v <- layer[[field]]
            uni <- unique(v)
            if(!all(uni %in% intervals) ) {
                vals <- uni[!uni %in% intervals]
                warning('Some values in the spatial data do not have an associated state in the network node.',
                        'The following values will be masked out: ', paste(vals,collapse=', '))
                layer[[field]][v %in% vals] <- NA
                uni <- uni[!uni %in% vals]
                nm <- field
                layer <- layer[field]
            }
        }
        ##
        # if(is.null(intervals)){
        #     intervals <- as.numeric(levels(v))
        #     warning('For categorical data check classes integer value and corresponding states. If not matching,',
        #             ' a look up list should be provided (function "setClasses") or modified from current list.')
        # }
    }
    lst <- list(list(States = states, 
                     Categorical = categorical, 
                     ClassBoundaries = intervals, 
                     SpatialData = layer)
                ) 
    names(lst) <- node
    if(verbose == TRUE){
        writeLines(c(paste('\n"', node, '"', ' points to:', sep=''), 
                     paste(' -> ', nm, '\n'), 
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
.loadSpatial <- function(item, field=NULL){ # TODO This should avoid loading the shape if field is missing
    if(!'RasterLayer' %in% class(item)){
        item <- tryCatch({
                rgdal::GDALinfo(item) # throws error if not raster
                return( raster::raster(item) )
            }, error=function(e){
                if(is.null(field)){
                    stop('"field" argument missing. Using vectorial data (e.g. shapefiles) one field/column ',
                         'for each corresponding node must be specified from the attribute table.')
                }
                .checkFields(item, field)
                if('SpatialPolygonsDataFrame' %in% class(item) | 'SpatialPointsDataFrame' %in% class(item)){
                    sf::st_as_sf(item) # transform from sp to sf
                } else {
                    sf::st_read(item, quiet = TRUE) # query = paste("SELECT", fld, "FROM", item)) )
                }
        } )
    }
    return(item) 
}
####
.checkFields <- function(item, fields){
    if(is.character(item)){
        nms <- rgdal::ogrInfo(item)$iteminfo$name
    } else {
        nms <- names(item)
    }
    if(!all(fields %in% nms)){
        f <- fields[!fields %in% nms]
        stop(paste(f, collapse=','),' missing from attribute table of spatial input data')
    }
}

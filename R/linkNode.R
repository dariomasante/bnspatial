#' @name linkNode
#' @title Link nodes to spatial data
#' 
#' @description \code{linkNode} links a node of the Bayesian network to the corresponding spatial data, returning a list of objects, 
#' including the spatial data and relevant info about the node.\cr
#' \code{linkMultiple} operates on multiple nodes and related spatial data.
#' @aliases linkMultiple
#' @param layer	character (path to file) or an object of class "RasterLayer", 
#' "sf" or "SpatialPolygonsDataFrame". The spatial data corresponding to the network node as by argument \code{node}.
#' @inheritParams loadNetwork
#' @param node character. The network node to be coupled with the file/object indicated by \code{layer} argument
#' @param intervals A list of numeric vectors. For categorical variables the spatial data values associated to each state of the node, 
#' for continuous variables the boundary values dividing into the corresponding states, including upper and lower boundaries.
#' @param categorical logical, or NULL. Is the node a categorical variable? If NULL the function will attempt to assign the 
#' logical value by looking at \code{intervals} argument.
#' @param field character. Only for spatial vector data (e.g. shapefile), the field/column names in 
#' the attribute table corresponding to the nodes, ordered accordingly.
#' @param verbose logical. If \code{verbose = TRUE} a summary of class boundaries and associated nodes and data will be printed 
#' to screen for checks.
#' @param spatialData character with path to one or more raster files or to a single spatial vector file, or a list of objects of 
#' class 'RasterLayer' (for raster), or a single object of class 'sf' or 'SpatialPolygonsDataFrame' (for spatial vector). 
#' The spatial data associated to given network nodes, provided as file paths or as (list of) spatial objects. Items must be ordered 
#' accordingly to the corresponding nodes listed in \code{lookup}, 
#' or provided as named list, where names match exactly to the corresponding nodes name. 
#' @param lookup character (path to file) or a formatted list. This argument can be provided as path to a comma separated file or a formatted list (see \code{\link{setClasses}} )
#' @param spatial logical. Should the output list include the input as raster or spatial vector? 
#' Default is TRUE, returning the list with an object of class "RasterLayer" or "sf" for each node associated. 
#' If FALSE, returns only the values.
#' @return \code{linkNode} returns a list of objects, including the spatial data and the related node information. \cr
#' \code{linkMultiple} returns a list of lists. Each element of the list includes the spatial data and summary information for each of the input nodes.
#' @details In future releases, this function may be rewritten to provide an S4/S3 object.
#' @seealso \code{\link{dataDiscretize}}; \code{\link{setClasses}}
#' @examples
#' ## Load data into global environment
#' list2env(ConwyData, environment())
#' lookup <- LUclasses
#' 
#' network <- LandUseChange
#' ln <- linkNode(layer=ConwyLU, network, node='CurrentLULC', intervals=c(2, 3, 1))
#' ln
#'
#' ## Link the Bayesian network to multiple spatial data at once, using a lookup list
#' spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#' linkMultiple(spatialData, network, lookup, verbose = FALSE)
#' 
#' ## Method for spatial vectorial data (i.e. class 'sf' or 'SpatialPolygon')
#' spatialData <- system.file("extdata", "Conwy.shp", package = "bnspatial")
#' lst <- linkMultiple(spatialData, network, lookup, field= c('LU', 'Slope', 'Status'))
#' lst
#' 
#' @export
linkNode <- function(layer, network, node, intervals, categorical=NULL, field=NULL, verbose=TRUE, spatial=TRUE){
    if(length(field) > 1) stop('Argument "field" must be a single item.')
    if(length(node) > 1) stop('Argument "node" must be a single item.')
    # Check correspondence of number of states and intervals
    if(!identical(intervals, unique(intervals))){
        stop('Non unique intervals defined')
    }
    network <- loadNetwork(network=network)
    # Check correspondence of node and states names between lookup list and network
    .checkNames(network, node)
    states <- network$universe$levels[[node]]
    delta <- length(intervals) - length(states)
    if(!delta %in% c(0, 1)){
        stop('Number of classes does not match number of states.')
    }
    if(!is.null(categorical)){
        if(!is.logical(categorical)) stop('Argument "categorical" must be TRUE, FALSE or NULL.')
        if(categorical & delta != 0) {
            stop('Number of classes does not match number of states.')
        } else if(categorical == FALSE & delta != 1) {
            stop('Number of intervals does not match number of states. For non categorical data ', 
                 'outer boundaries (minimum and maximum) must be set (-Inf and Inf allowed).')
        }
    }
    categorical <- ifelse(delta != 0, FALSE, TRUE)
    if( identical(intervals, sort(intervals)) == FALSE & categorical == FALSE){
        stop('"intervals" must be sorted from lowest to highest.')
    }
    layer <- .loadSpatial(layer, field)
    if('RasterLayer' %in% class(layer)){
        nm <- names(layer)
        if(categorical) {
            uni <- raster::unique(layer)
            if(!all(uni %in% intervals) ) {
                vals <- uni[!uni %in% intervals]
                warning('Some values in the spatial data do not have an associated state in the network node. ',
                        'The following values will be treated as NA: ', 
                        paste(utils::head(vals, 20),collapse=', '))
                rcl <- cbind(from=vals, to=NA)
                layer <- raster::reclassify(layer, rcl)
                uni <- raster::unique(layer)
            }
            if(raster::is.factor(layer)) { # Check values correspond when raster table available
                df <- raster::levels(layer)[[1]]
                m <- data.frame(intervals, states, stringsAsFactors = FALSE)
                mm <- merge(df, m, by.x='ID', by.y='intervals')
                a <- !apply(mm, 1, function(x){x[2] == x[3] }) # check unmatching
                vals <- mm[a, ]
                if(!all(a)){
                    stop('Some values in categorical spatial data and associated states do not match: ',
                         'values ', paste(vals[,1], collapse=', '), ' correspond to state "', paste(vals[,2], collapse=', '), 
                         '" in network node "', node, '", but to "', paste(vals[,3], collapse=', '), 
                         '" in associated spatial data "', nm,'"')
                }
            } else {
                layer <- raster::ratify(layer)
                df <- data.frame(ID=raster::levels(layer)[[1]])
                df$VALUE <- states[match(df$ID, intervals)]
                levels(layer) <- df
            }
        }
        if(!spatial){
            layer <- raster::getValues(layer)
        }
    } else {
        v <- layer[[field]]
        nm <- field
        if(categorical){
            uni <- !is.element(v, intervals)
            if(any(uni) ) {
                warning('Some values in the spatial data do not have an associated state in the network node.\n',
                        'The following values of "',field,'" will be treated as NA: ', paste(unique(v[uni]),collapse=', '))
                v[uni] <- NA
            }
        } else {
            uni <- (v < intervals[1] | v > intervals[length(intervals)])
            if(any(uni, na.rm=TRUE)) {
                warning('Some values in the spatial data do not have an associated state in the network node.\n',
                        'Values of "',field,'" lower than ',intervals[1],' and higher than ',
                        intervals[length(intervals)],' will be treated as NA.')
                v[uni] <- NA
            }
        }
        if(spatial){
            layer <- layer[field] 
            layer[,1] <- v
        } else {
            layer <- v
        }
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
    if(verbose == TRUE) .verbose(node, nm, states, categorical, intervals)
    return(lst)
}

#' @rdname linkNode
#' @export
linkMultiple <- function(spatialData, network, lookup, field=NULL, verbose=TRUE){
    lookup <- .loadLookup(lookup)
    if(is.character(spatialData)){
        spatialData <- .loadSpatial(spatialData, field)
    }
    if('sf' %in% class(spatialData)){
        if(length(lookup) != length(field)) {
            stop('Look up list has ',length(lookup),' elements, while field has ', length(field),' elements. They must be of same length.')
        }
    } else {
        if(is.list(spatialData) & length(lookup) != length(spatialData) ) {
            stop('Look up list has ',length(lookup),' elements, while spatial data list has ', length(spatialData),' elements. They must be of same length.')
        }
        if('RasterLayer' %in% class(spatialData) & length(lookup) != 1){
            stop('Look up list has ',length(lookup),' elements, while spatial data list is one element only. They must be of same length.')
        }
    }
    lst <- list()
    namedVars <- sapply(names(lookup), function(nm){
        n <- which(names(spatialData) == nm)
        ifelse(length(n) > 0, n, 0)
    })
    if(all(namedVars > 0)){
        message('Node names and names from spatial data fully correspond, so they will be matched accordingly.')
    }
    network <- loadNetwork(network)
    for(nm in names(lookup)){
        .checkStates(lookup[[nm]]$States, network$universe$levels[[nm]], nm)
        Categorical <- lookup[[nm]]$Categorical
        if(Categorical == TRUE){
            sortedClasses <- match(network$universe$levels[[nm]],lookup[[nm]]$States)
            ClassBoundaries <- lookup[[nm]]$ClassBoundaries[sortedClasses]
        } else {
            ClassBoundaries <- lookup[[nm]]$ClassBoundaries
        }
        if('sf' %in% class(spatialData)){
            if(all(namedVars > 0)){ 
                spd <- spatialData[ namedVars[nm] ]
            } else {
                # if(is.null(lookup[[nm]]$layer) ){
                    n <- which(names(lookup) == nm)
                    spd <- spatialData[field[n]] # associate by simple order of list
                # } else {
                #     spd <- lookup[[nm]]$layer # disabled
                # }
            }
            lst[nm] <- linkNode(spd, network=network, node=nm, 
                                intervals=ClassBoundaries, categorical=Categorical, 
                                field=names(spd)[1], verbose=verbose, spatial=FALSE)
        } else {
            if(all(namedVars > 0)){ 
                spd <- spatialData[[ namedVars[nm] ]]
            } else {
                # if(is.null(lookup[[nm]]$layer) ){
                    n <- which(names(lookup) == nm)
                    spd <- spatialData[[n]] # associate by simple order of list
                # } else {
                #     spd <- lookup[[nm]]$layer # disabled
                # }
            }
            lst[nm] <- linkNode(spd, network=network, node=nm, intervals=ClassBoundaries, verbose=verbose,
                                categorical=Categorical, field=setdiff(colnames(spd), 'geometry'))
        }
    }
    if('sf' %in% class(spatialData)){
        lst$SpatialData <- .aoiVector(spatialData['geometry'])
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
    if(any(check == FALSE)){
        msg <- ifelse(is.null(node), '.', paste0(': ', '"', node, '"'))
        stop(paste('Names of states provided do not match the node states from the network: \n"', 
                   inputStates[!check], '" missing from network node', msg))
    }
}
####
.loadSpatial <- function(item, field=NULL, checkfld=TRUE){
    ck <- c('SpatialPolygonsDataFrame','SpatialPointsDataFrame','sf','sfc') # vector objects
    stopstring <- paste('"field" argument missing. Using vectorial data (e.g. shapefiles) one field/column',
    'from the attribute table must be specified for each corresponding node.')
    if(!checkfld) field <- 1 # this is for "aoi" function
    if(any(ck %in% class(item))){
        if(checkfld){
            if(is.null(field)) stop(stopstring)
            .checkFields(item, field)
        }
        if(any(ck[1:2] %in% class(item))){
            item <- sf::st_as_sf(item)[field] # transform from sp to sf
        }
    } else if(is.list(item)){
        ck <- sapply(item, function(x) 'RasterLayer' %in% class(x))
        if(!all(ck)) stop('Input spatial data not appropriate. When input is a list, it must contain only RasterLayer objects.')
        if(!is.null(field) & checkfld) message('Spatial data in raster format, "field" argument will be ignored.')
        .checkSR(item)
    } else if('RasterLayer' %in% class(item)){
        # do nothing for now, but at least avoid final else statement
    } else if(is.character(item)){
        tc <- tryCatch( # if not raster returns NULL
            lapply(item, function(x){
                suppressWarnings(rgdal::GDALinfo(x))
                raster::raster(x)
            }), error=function(e){
                s <- ifelse(file.exists(item), FALSE, TRUE)
                if(any(s)) stop(paste(item[s], collapse=' ; '), '  \n File not found!')
        })
        .checkSR(tc)
        if(is.null(tc)){
            if(length(item) != 1) stop('For vectorial spatial data, a single data object is required, with all the necessary attributes.')
            if(checkfld & is.null(field)) stop(stopstring)
            p <- sf::st_read(item, quiet = TRUE, fid_column_name='FID')
            tc <- p[field] # tc <- lapply(field, function(f) { p[f] })
        }
        if(length(tc) == 1) { tc <- tc[[1]] }
        return(tc)
    } else {
        stop('Input spatial data do not correspond to any allowed object classes (character, sf, RasterLayer, ',
             'SpatialPolygonsDataFrame, SpatialPointsDataFrame). Please check function help.')
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
        stop(paste(f, collapse=', '),' missing from attribute table of spatial input data.')
    }
}
####
.checkSR <- function(rastList){
    if(length(rastList) > 1){
        sr <- raster::crs(rastList[[1]])
        sapply(rastList, function(x) {
            if(!raster::compareCRS(raster::crs(x), sr)){ 
                stop('Multiple reference systems found, or some missing. Please convert/set all input spatial data to a single reference system.') 
            }
        })
    }
}
####
.verbose <- function(node, nm, states, categorical, intervals){
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

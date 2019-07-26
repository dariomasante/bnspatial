#' bnspatial
#' @title Spatialize the Bayesian network
#' 
#' @description This function wraps most bnspatial package functions to ease the spatial implementation of Bayesian networks with minimal user interaction.
#' @inheritParams loadNetwork
#' @inheritParams aoi
#' @inheritParams mapTarget
#' @inheritParams linkNode
#' @inheritParams queryNet
#' @param inparallel logical or integer. Should the function use parallel processing facilities? Default is FALSE: a single process will be launched. If TRUE, all cores/processors but one will be used.
#' Alternatively, an integer can be provided to dictate the number of cores/processors to be used. 
#' @return A list of "RasterLayer" or "SpatialPolygonsDataFrame" objects or a data.frame, depending on input arguments: see \code{\link{mapTarget}}. 
#' Some basic information about discretization and network/data link are printed on screen during execution.
#' @details The expected value is calculated by summing the mid values of target node states weighted by their probability: 
#' \code{p1 * midVal_1 + p2 * midval_2 + ... + pn * midval_n}\cr
#' When a spatial object is exported to a file, the file name is set by default, accordingly to the following naming convention:
#' \itemize{
#' \item{\code{"class"}} \emph{<target node name>}_Class.\emph{<file format  -default .tif>}
#' \item{\code{"entropy"}} \emph{<target node name>}_ShanEntropy.\emph{<file format  -default .tif>}
#' \item{\code{"probability"}} \emph{<target node name>}_Probability_.\emph{<targetState>}.\emph{<file format  -default .tif>}
#' \item{\code{"expected"}} \emph{<target node name>}_ExpectedValue.\emph{<file format  -default .tif>}
#' \item{\code{"variation"}} \emph{<target node name>}_CoefVariation.\emph{<file format  -default .tif>}
#' }
#' An additional comma separated file (.csv) is written to the same directory when \code{"class"}, 
#' providing a key to interpret the spatial object values and the state they refer to.
#' @seealso \code{\link{setClasses}}; \code{\link{mapTarget}}; \code{\link{linkNode}}; \code{\link{loadNetwork}}
#' @examples
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#' lookup <- LUclasses
#' 
#' bn <- bnspatial(network, 'FinalLULC', spatialData, lookup)
#' bn
#' 
#' @export
bnspatial <- function(network, target, spatialData, lookup, msk=NULL, what=c("class", "entropy"), 
                      midvals=NULL, targetState=NULL, spatial=TRUE, inparallel=FALSE, 
                      export=FALSE, path=NULL, field=NULL, verbose=TRUE, ..., exportRaster=FALSE){
    network <- loadNetwork(network, target)
    
    ## Load table with class boundaries, if available (otherwise make a list with node name and associated vector of class boundaries)
    lookup <- .loadLookup(lookup)
    
    ## Load input spatial data and corresponding nodes and states into a list
    spatialDataList <- linkMultiple(spatialData=spatialData, network=network, 
                                    lookup=lookup, field=field, verbose=verbose)
    
    ## Remove spatial data that was set as evidence in the ellipsis (...) or is the target
    spatialDataList <- .removeEllipsis(spatialDataList, network, target, ...)
    # lookup <- lapply(spatialDataList, function(x) x[!names(x) %in% "SpatialData"])
    
    ## Load or create mask
    is.sf <- 'sf' %in% class(spatialDataList$SpatialData)
    if(is.numeric(msk)) {
        bbox <- msk
        msk <- NULL
    } else {
        bbox <- NULL
    }
    if(is.null(msk)){
        if(is.sf){
            msk <- aoi(spatialDataList$SpatialData) 
        } else {
            msk <- aoi( lapply(spatialDataList,'[[',4) ) 
        }
    } else {
        msk <- aoi(msk, bbox=bbox)
        if(is.sf){
            its <- sf::st_intersection(spatialDataList$SpatialData, msk) 
            its <- sf::st_collection_extract(its, type = "POLYGON")
            for(nm in names(spatialDataList)){
                spatialDataList[[nm]]$SpatialData <- spatialDataList[[nm]]$SpatialData[its$FID]
            }
        }
    }
    xyMsk <- aoi(msk, xy=TRUE)
    ## Extract data from locations, discretize and query Bayesian network
    if(inparallel != FALSE){ ## Leave != FALSE to avoid confusion between '== 1' and '== TRUE'
        inparallel <- .inParallel(inparallel)
        tokenToHaltChildrenFromParallelProc <- NULL ## Trick to avoid making cluster twice in the functions below 
        clst <- parallel::makeCluster(inparallel)
        doParallel::registerDoParallel(clst)
        tab <- bulkDiscretize(formattedLst = spatialDataList, xyMsk, inparallel=inparallel)
        probs <- queryNetParallel(network=network, target=target, evidence=tab, inparallel=inparallel, ...)
        parallel::stopCluster(clst)
    } else {
        if(is.sf){
            spatialDataList['SpatialData'] <- NULL
            tab <- matrix(nrow=length(xyMsk), ncol=length(spatialDataList))
        } else {
            tab <- matrix(nrow=nrow(xyMsk), ncol=length(spatialDataList))   
        }
        colnames(tab) <- names(spatialDataList)
        for(nm in colnames(tab)) {
            if(is.sf){
                ex <- spatialDataList[[nm]]$SpatialData[xyMsk]
            } else {
                layer <- spatialDataList[[nm]]$SpatialData
                ex <- extractByMask(layer, msk=xyMsk)
            }
            if(spatialDataList[[nm]]$Categorical == TRUE){
                tab[, nm] <- spatialDataList[[nm]]$States[match(ex, spatialDataList[[nm]]$ClassBoundaries)]
            } else {
                tab[, nm] <- dataDiscretize(ex, spatialDataList[[nm]]$ClassBoundaries, spatialDataList[[nm]]$States)[[1]]
            }
        }
        probs <- queryNet(network=network, target=target, evidence=tab, ...)
    }
    m <- mapTarget(target=target, statesProb=probs, what=what, msk=msk, midvals=midvals, 
              spatial=spatial, targetState=targetState, export=exportRaster, path=path)
    if(!is.null(m$classLegend) & target %in% names(lookup)) { # remap values of classes, if in lookup
        itm <- lookup[[target]]
        if(itm$Categorical){
            m$classLegend$remap <- itm$ClassBoundaries[match(itm$States, m$classLegend[[target]])]
            if(is.sf){
                # vals <- m$Class
                # m$Class <- m$classLegend$remap[match(vals, m$classLegend$cell_ID)]
            } else {
                vals <- raster::getValues(m$Class)
                m$Class <- raster::setValues(m$Class, m$classLegend$remap[match(vals, m$classLegend$cell_ID)])
            }
            m$classLegend$cell_ID <- m$classLegend$remap
            m$classLegend <- m$classLegend[,1:2]
        }
    }
    return(m)
}

##
.loadLookup <- function(lookup){
    if(is.character(lookup) & length(lookup) == 1){
        return( importClasses(classFile=lookup) )
    } else if (is.list(lookup) & length(lookup[[1]]) == 3 & is.list(lookup[[1]])){
        return( lookup )
    } else {
        stop('Check "lookup": must be a text file or a formatted list as output from "setClasses" and "importClasses" functions')
    }
}

##
.removeEllipsis <- function(spdl, network, target, ...){
    if(length(list(...)) > 0){
        l <- list(...)
        rnm <- c(names(l), target)
        itsct <- intersect(names(spdl), rnm)
        sapply(itsct, function(x){
            .checkStates(l[[x]], network$universe$levels[[x]], node=x)
        })
        spdl <- spdl[!names(spdl) %in% itsct]
    }
    return(spdl)
}
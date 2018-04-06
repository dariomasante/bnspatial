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
                      exportRaster=FALSE, path=NULL, verbose=TRUE, ...){
    network <- loadNetwork(network, target)
    
    ## Load table with class boundaries, if available (otherwise make a list with node name and associated vector of class boundaries)
    if(is.character(lookup) & length(lookup) == 1){
        lookup <- importClasses(classFile=lookup)
    } else if (is.list(lookup) & length(lookup[[1]]) == 3 & is.list(lookup[[1]])){
        lookup <- lookup
    } else {
        stop('Check "lookup": must be a text file or a formatted list as output from "setClasses" and "importClasses" functions')
    }
    
    ## Load input spatial data and corresponding nodes and states into a list
    if(length(spatialData) != length(lookup)){
        stop('Check "spatialData": must be a vector of file names or a list of "RasterLayer" or "SpatialPolygonsDataFrame" ', 
             'objects of length equal to the number of nodes provided by lookup argument')
    }
    spatialDataList <- linkMultiple(spatialData=spatialData, network=network, lookup=lookup, verbose=verbose)
    
    ## Load or create mask
    if(is.null(msk)){
        msk <- aoi(spatialData)
    } else {
        msk <- aoi(msk)
    }
    xyMsk <- aoi(msk, xy=TRUE)
    
    #	## Remove spatial data that was set as evidence in the ellipsis (...) or is the target (currently done by queryNet, but rather inefficient)
    #	if(length(list(...)) > 0){
    #	ls() 
    #		if(ls()... %in% names(spatialDataList)){
    #			spatialDataList = spatialDataList[-which(... %in% names(spatialDataList))]
    #		}
    #	}
    
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
        tab <- matrix(nrow=nrow(xyMsk), ncol=length(spatialDataList))
        colnames(tab) <- names(spatialDataList)
        for(nm in colnames(tab)) {
            rst <- spatialDataList[[nm]]$Raster
            ex <- extractByMask(rast=rst, msk=xyMsk)
            if(spatialDataList[[nm]]$Categorical == TRUE){
                tab[, nm] <- spatialDataList[[nm]]$States[match(ex, spatialDataList[[nm]]$ClassBoundaries)]
            } else {
                tab[, nm] <- dataDiscretize(ex, spatialDataList[[nm]]$ClassBoundaries, spatialDataList[[nm]]$States)[[1]]
            }
        }
        probs <- queryNet(network=network, target=target, evidence=tab, ...)
    }
    mapTarget(target=target, statesProb=probs, what=what, msk=msk, midvals=midvals, 
              spatial=spatial, targetState=targetState, exportRaster=exportRaster, path=path)
}


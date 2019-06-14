#' @name aoi
#' @title Build area of interest (A.O.I.)
#'
#' @description This function creates a spatial object defining the area of interest, by unioning the input spatial data or using a user defined mask.
#' When \code{msk} is specified and is a raster, extent is set equal to it, but resolution 
#' to the finest resolution among that and input spatial data. If not specified, it will union the extents of input spatial data.
#' @param msk a character (path to raster/vector file), a list of rasters of class "RasterLayer", or a single object of class "sf" or "SpatialPolygonsDataFrame". 
#' The reference data (raster or vector) to be used as mask. All model outputs will have the same extent and outline as this object. 
#' All locations with no data (i.e. NA) cells in \code{msk} input will be ignored as well.
#' @param mskSub vector of values. The subset values from \code{msk} which should be considered to build the area of interest. All other values will be ignored and returned as NA.
#' @param xy logical. Should return a two column matrix of coordinates? If FALSE an object of class RasterLayer is returned.
#' @details All model outputs will have the same resolution and same extent as inherited from \code{msk}. All locations with no data (i.e. NA) cells 
#' from \code{msk} will be ignored as well.
#' @return An object of class RasterLayer (default), or a matrix of coordinates of mask cells. In the former case, valid cells (i.e. the area of interest) will have value 1, \code{NA} otherwise.
#' @seealso \code{\link{extractByMask}}
#' @examples
#' ## Make a mask from a group of input layers:
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#' m <- aoi(spatialData)
#' m
#' 
#' ## Plot mask
#' library(raster)
#' m <- aoi(ConwyLU)
#' plot(m)
#' 
#' ## Make mask from a subset of values and plot
#' m <- aoi(ConwyLU, mskSub=c(2,3))
#' plot(m)
#' 
#' ## Return coordinates of valid mask locations
#' coord <- aoi(ConwyLU, xy=TRUE)
#' head(coord)
#' @export
aoi <- function(msk, mskSub=NULL, xy=FALSE, ext=NULL){  ## Check if aoi and extractByMask can be condensed in one or nested.
    msk <- .loadSpatial(msk, checkfld=FALSE)
    # if(class(msk) == 'character'){
    #     msk <- lapply(msk, function(x) { raster::raster(x, RAT=FALSE) }) ## Should apply this even when an input raster is provided
    # }
    if(is.list(msk)){ 
        r <- msk[[1]]
        sr <- raster::crs(r)
        ext <- raster::extent(r)
        cellSizeX <- raster::res(r)[1]
        cellSizeY <- raster::res(r)[2]
        if(length(msk) > 1){
            for(i in 2:length(msk)){
                r <- msk[[i]]
                if(!raster::compareCRS(raster::crs(r), sr)){ stop('Multiple reference systems found. Please convert all data to a single one.') }
                ext <- raster::union(ext, raster::extent(r) )
                cellSizeX <- ifelse(cellSizeX > raster::res(r)[1], raster::res(r)[1], cellSizeX)
                cellSizeY <- ifelse(cellSizeY > raster::res(r)[2], raster::res(r)[2], cellSizeY)
            }
        }
        msk <- raster::raster(ext=ext, resolution=c(cellSizeX, cellSizeY), crs=sr, vals = 1) 
        if(xy == TRUE){
            return( raster::xyFromCell(msk, seq_along(msk)) )
        } else {
            return( msk )
        }
    } else {
        mskVals <- raster::getValues(msk)
        if(!is.null(mskSub)){
            id = mskVals %in% mskSub
        } else {
            id = is.finite(mskVals)
        }
        if(xy == TRUE) {
            id = seq_along(msk)[id]
            return( raster::xyFromCell(msk, id) )
        } else {
            msk[] <- NA
            msk[id] <- 1
            names(msk) <- 'layer'
            return( msk )
        }
    }
}

##
.aoiRaster <- function(msk, mskSub, xy, ext){
    
}

##
.aoiVector <- function(msk, mskSub, xy, ext){ # use xy to return id?
    
}

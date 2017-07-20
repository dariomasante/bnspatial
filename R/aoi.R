#' @name aoi
#' @title Build area of interest (A.O.I.)
#'
#' @description This function creates a raster defining the area of interest, by unioning the input rasters or using a user defined mask.
#' When \code{msk} is specified, resolution and extent are set equal to it, otherwise
#' to the finest resolution among input spatial data and unioning the extents of input spatial data.
#' @param msk a character (path to raster file), a raster (object of class "RasterLayer"), or a list of rasters. 
#' The reference raster(s) to be used as mask. All model outputs will have the same resolution and same extent as this raster(s). 
#' All locations with no data (i.e. NA) cells in \code{msk} will be ignored as well.
#' @param mskSub vector. The subset values from \code{msk} which should be considered to build the area of interest. All other values will be ignored and returned as NA.
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
aoi <- function(msk, mskSub=NULL, xy=FALSE){  ## Check if aoi and extractByMask can be condensed in one or nested.
    if(class(msk) == 'character'){
        msk <- lapply(msk, function(x) { raster::raster(x, RAT=FALSE) }) ## Should apply this even when an input raster is provided
    }
    if(is.list(msk)){ 
        r <- msk[[1]]
        ext <- raster::extent(r)
        cellSizeX <- raster::res(r)[1]
        cellSizeY <- raster::res(r)[2]
        if(length(msk) > 1){
            for(i in 2:length(msk)){
                r <- msk[[i]]
                ext <- raster::union(ext, raster::extent(r) )
                cellSizeX <- ifelse(cellSizeX > raster::res(r)[1], raster::res(r)[1], cellSizeX)
                cellSizeY <- ifelse(cellSizeY > raster::res(r)[2], raster::res(r)[2], cellSizeY)
            }
        }
        msk <- raster::raster(ext=ext, resolution=c(cellSizeX, cellSizeY), vals = 1) 
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
            return( msk )
        }
    }
}

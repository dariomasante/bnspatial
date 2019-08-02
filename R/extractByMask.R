#' @name extractByMask
#' @title Extract raster values by mask
#' @description This function extracts the values from a given input raster based on a mask.
#' @param layer an object of class "RasterLayer" (package \href{https://cran.r-project.org/package=raster}{raster}). The raster from which data will be extracted
#' @param msk an object of class "RasterLayer" or a two column matrix of coordinates. The reference raster (or coordinates) to be used as mask for extraction.
#' @param spatial logical. Should the output be spatially explicit -i.e. a georeferenced raster? 
#' Default is FALSE, returning a vector of extracted values from \code{rast}. 
#' If TRUE an object of class "RasterLayer" is returned.
#' @param rast deprecated, use \code{layer} instead.
#' @return a vector, or an object of class "RasterLayer". The values from the input raster (\code{rast} argument) at coordinates provided as matrix, or those overlapping with non NA cells in the mask raster. If \code{spatial == TRUE} an object of class "RasterLayer" is returned.
#' @details When input data given to \code{rast} does not match the resolution and extent of a raster mask argument, the latter is preferred. The function will therefore return a vector of n elements, one for each non NA cell in the mask. Input raster cells falling inside mask cells, but not over their cells centre will be ignored.
#' @seealso \code{\link{aoi}}
#' @examples
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' m <- aoi(msk=ConwyLU, mskSub=c(2,3))
#' head( extractByMask(ConwySlope, msk=m), 20) 
#' 
#' # Extract making a raster
#' library(raster)
#' plot( extractByMask(ConwySlope, msk=m, spatial=TRUE) )
#' @export
extractByMask <- function(layer, msk, spatial=FALSE, rast=NULL){
    if (!missing('rast')) {
        warning('argument "rast" is deprecated; please use "layer" instead.', call. = FALSE)
        layer <- rast
    }
    if(!any(c('RasterLayer', 'sf') %in% class(layer))){
        stop('"layer" argument must be an object of class "RasterLayer" or "sf".')
    }
    if('RasterLayer' %in% class(msk) | is.matrix(msk)){
        .extRaster(msk, layer, spatial)
    } else if ('sf' %in% class(msk) | is.integer(msk)){
        .extVector(msk, layer, spatial)
    } else {
        stop('"msk" argument must be either an object of class "RasterLayer", "sf", ',
             'or a two column matrix of x and y coordinates, or of features ID (FID)')
    }
}

###
.extRaster <- function(msk, rast, spatial){
    if(is.matrix(msk)){
        cells <- id <- raster::cellFromXY(rast, msk[, 1:2])
        msk <- rast
        msk[] <- NA
    } else {
        id <- .whichValidCells(msk)
        xy <- raster::xyFromCell(msk, id)
        cells <- raster::cellFromXY(rast, xy[, 1:2])
    }
    vals <- raster::getValues(rast)[cells]
    if(spatial == TRUE){
        msk[id] <- vals
        return(msk)
    } else {
        return(vals)
    }
}
###
.extVector <- function(msk, vct, spatial){
    if(is.integer(msk)){
        vct <- vct[msk, ]
    } else {
        vct <- sf::st_intersection(vct, msk)
    }
    if(spatial != TRUE){
        vct <- as.data.frame(vct)[ ,setdiff(colnames(vct), 'geometry')]
    }
    return(vct)
}
###
.whichValidCells <- function(r){
    v <- raster::getValues(r)
    id <- seq_along(v)
    id[is.finite(v)]
}

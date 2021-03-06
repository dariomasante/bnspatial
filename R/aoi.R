#' @name aoi
#' @title Build area of interest (A.O.I.)
#'
#' @description This function creates a spatial object (raster or vector) defining the area of interest, by taking a bounding box or 
#' a spatial object, or unioning the input spatial objects if more than one are provided.
#' When \code{msk} is a list of rasters, extent is set equal to their combined extent (union) and resolution 
#' to the finest resolution among them. 
#' @param msk a character (path to raster or vector file), or a bounding box as numeric (xmin,xmax,ymin,ymax), 
#' or one or more (as list of) rasters of class "RasterLayer", or a single object of class "sf" or "SpatialPolygonsDataFrame". 
#' The reference data (raster or vector) to be used as mask. All model outputs will have the same extent (outline) as this object. 
#' All locations with no data (i.e. NA) cells in \code{msk} input will be ignored as well.
#' @param mskSub vector of values, for raster data only. The subset values from \code{msk} which should be considered to build the area 
#' of interest. All other values will be ignored and masked out during modelling.
#' @param xy logical. Should return a two column matrix of x and y coordinates of cells centre (raster data) or
#' the IDs of features? Defaults to FALSE, returning an object of class RasterLayer or sf.
#' @param bbox numeric of four elements, the coordinates defining a rectangle (bounding box) to limit the area of interest. 
#' Must be ordered as xmin, xmax, ymin, ymax. Coordinates must be in the same reference system as spatial data.
#' @details If rasters are used, all model outputs will have the same resolution and same extent as inherited from \code{msk}. 
#' All locations with no data (i.e. NA) cells from \code{msk} will be ignored as well.
#' @return An object of class RasterLayer  or sf, or a matrix of coordinates of mask cells (raster only). In the former case, valid cells 
#' (i.e. the area of interest) will have value 1, \code{NA} otherwise.
#' @seealso \code{\link{extractByMask}}
#' @examples
#' ## Make a mask from a group of input layers:
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
#' 
#' ## Using a bounding box
#' aoi(ConwyLU, bbox=c(270000, 284950, 347000, 365000))
#' 
#' ## For vectorial spatial data. Note xy=TRUE shall return the features IDs
#' Conwy = sf::st_read(system.file("extdata", "Conwy.shp", package = "bnspatial"))
#' aoi(Conwy, bbox=c(270000, 284950, 347000, 365000))
#' 
#' @export
aoi <- function(msk, mskSub=NULL, xy=FALSE, bbox=NULL){  ## Check if aoi and extractByMask can be condensed in one or nested.
    msk <- .loadSpatial(msk, checkfld=FALSE)
    msk <- .bbox(bbox, msk)
    if('RasterLayer' %in% class(msk[[1]])){
        .aoiRaster(msk, mskSub, xy)
    } else {
        if(!is.null(mskSub)) warning('mskSub argument ignored for vectorial spatial data')
        .aoiVector(msk, xy) 
    }
}

##
.bbox <- function(bbox, msk){
    if(!is.null(bbox)){
        if(!is.numeric(bbox) | length(bbox) != 4) stop('Bounding box must be a numeric vector of length 4 (i.e xmin, xmax, ymin, ymax)')
        if('RasterLayer' %in% class(msk[[1]])){
            if(is.list(msk)){
                msk <- lapply(msk, function(m) { raster::crop(m, bbox) })    
            } else {
                msk <- raster::crop(msk, bbox)
            }
        } else {
            msk <- sf::st_crop(sf::st_buffer(msk, dist = 0), 
                                xmin=bbox[1],xmax=bbox[2],ymin=bbox[3],ymax=bbox[4])
        }
    }
    return(msk)
}
##
.aoiRaster <- function(msk, mskSub, xy){
    # if(class(msk) == 'character'){
    #     msk <- lapply(msk, function(x) { raster::raster(x, RAT=FALSE) }) ## Should apply this even when an input raster is provided
    # }
    if(is.list(msk)){ 
        .checkSR(msk)
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
        msk <- raster::raster(ext=ext, resolution=c(cellSizeX, cellSizeY), crs=raster::crs(r), vals = 1) 
        if(xy == TRUE){
            return( raster::xyFromCell(msk, seq_along(msk)) )
        } else {
            return( msk )
        }
# TODO cope with lower resolution raster
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
.aoiVector <- function(msk, xy=FALSE){ # use xy to return id?
    if(!'FID' %in% names(msk)) {
        # warning('No features ID found in attributes of "msk" (FID column); ', 
        #         'a column named "FID" will be guessed from ',
        #         'attributes table, with unique integers for each feature.')
        msk["FID"] <- 1:nrow(msk)
    }
    if(xy){
        return(msk[['FID']])
    } else {
        return(msk['FID'])
    }
}

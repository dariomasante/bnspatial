#' mapTarget
#' @title Make maps for target node
#'
#' @description This function creates the required spatial outputs for the target node.
#' @param target character. The node of interest to be modelled and mapped.
#' @param statesProb matrix. The probability matrix as returned by \code{\link{queryNet}} and \code{queryNetParallel}. 
#' Columns must be named accordingly to states of the target node.
#' Columns are the \code{target} node states and rows each location considered from the area of interest.
#' @param what character. The required output, one or more of these are valid:
#' \itemize{
#' \item{\code{"class"}} returns the relatively  most likely state.
#' \item{\code{"entropy"}} calculates the Shannon index and returns the entropy, given the node probabilities.
#' \item{\code{"probability"}} returns an object for each state of the target node, with its probability.
#' \item{\code{"expected"}} gives the expected value for the target node (see Details). Only valid for 
#' target nodes of continuous values. \code{midValues} argument must be provided.
#' \item{\code{"variation"}} returns the coefficient of variation, as a measure of uncertainty. 
#' Only valid for target nodes of continuous values.
#' }
#' @param msk an object of class "RasterLayer" (raster) or a spatial vector of class "sf" (vector spatial polygons). 
#' The reference spatial boundaries to be used as mask. All model outputs will have the same extent/outline as this object. 
#' All locations with no data (i.e. NA) will be ignored.
#' @param midvals vector of length equal to the number of states of the target node. 
#' Applies only if the target node is a continuous variable, in which case \code{midvals} must 
#' contain the mid values for each of the intervals 
#' @param targetState character. One or more states of interest from the target node. Applies only 
#' when argument \code{what} includes \code{'probability'}. Default is set to all states of the node.
#' @param spatial logical. Should the output be spatially explicit -i.e. a georeferenced raster or spatial vector? 
#' Default is TRUE, returning an object of class "RasterLayer" or "sf" for polygons. If FALSE, returns a data frame 
#' with one row for each valid cell/feature from \code{msk} and in columns the output required by \code{what} argument.
#' @param export Logical or character. Should the spatial output be exported to file? 
#' Applies only if argument \code{spatial=TRUE}. When \code{export=TRUE}, output will be 
#' exported in .tif (raster) or .shp (vector) format. For rasters, a character specifying another 
#' extension can be provided, in which case the 
#' raster will be exported in that format. Only formats listed by \link[raster]{writeFormats} are valid. 
#' Argument \code{exportRaster} is deprecated.
#' @param path The directory to store the output files, when \code{export} is not FALSE. 
#' Default is the working directory as from \code{getwd()}. File names are set by a default naming convention, see Details.
#' @param exportRaster deprecated, use \code{export} instead.
#' @return A list of objects, one for each item required in \code{what} argument. If \code{spatial = TRUE} 
#' a list of rasters of class "RasterLayer" are returned, or a single spatial vector of class "sf" with one column 
#' for each output requested. If FALSE, for raster data it returns a list of vectors with the values 
#' associated to each non NA cell in msk raster (i.e. the vectorised raster). For vector data it returns a data frame. 
#' If argument \code{export} is specified, outputs are exported to files to the directory specified in \code{path}.
#' @details The expected value is calculated by summing the mid values of target node states weighted by their probability: 
#' \code{p1 * midVal_1 + p2 * midval_2 + ... + pn * midval_n}\cr
#' When exporting to a file, the file name is set by default, according to the following naming convention:
#' \itemize{
#' \item{\code{"class"}} \emph{<target node name>}_Class.\emph{<file format  -default .tif>}
#' \item{\code{"entropy"}} \emph{<target node name>}_ShanEntropy.\emph{<file format  -default .tif>}
#' \item{\code{"probability"}} \emph{<target node name>}_Probability_.\emph{<targetState>}.\emph{<file format  -default .tif>}
#' \item{\code{"expected"}} \emph{<target node name>}_ExpectedValue.\emph{<file format  -default .tif>}
#' \item{\code{"variation"}} \emph{<target node name>}_CoefVariation.\emph{<file format  -default .tif>}
#' }
#' An additional comma separated file (.csv) is written to the same directory when \code{"class"}, 
#' providing a key to interpret the values and the state they refer to.
#' @seealso \code{\link{bnspatial}}, \code{\link{aoi}}, \code{\link{queryNet}}
#' @examples
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' target <- 'FinalLULC'
#' statesProb <- queryNet(network, target, evidence)
#' 
#' maps <- mapTarget(target, statesProb, msk=ConwyLU)
#' 
#' library(raster)
#' plot(maps$Class)
#' plot(maps$Entropy)
#' 
#' ## Returns required outputs by coordinates for each 'msk' cell in a data frame:
#' noMap <- mapTarget(target, statesProb, msk=ConwyLU, spatial=FALSE)
#' head(noMap)
#' 
#' ## Create a probability surface for the "forest" state of target node "FinalLULC"
#' mp <- mapTarget('FinalLULC', statesProb, what='probability', targetState='forest', msk=ConwyLU)
#' plot(mp$Probability$forest)
#' 
#' ## With spatial vector (totally made up data here, just for demo):
#' library(sf)
#' spVector <- st_read(system.file("extdata", "Conwy.shp", package = "bnspatial"))
#' ev <- evidence[1:nrow(spVector), ]
#' 
#' probs <- queryNet(network, 'FinalLULC', ev)
#' mp <- mapTarget('FinalLULC', statesProb=probs,
#'                 what=c('entropy', 'probability'), targetState='forest', msk=spVector)
#' 
#' @export
mapTarget <- function(target, statesProb, what=c("class", "entropy"), msk, midvals=NULL,
                      targetState=NULL, spatial=TRUE, export=FALSE, path=getwd(), exportRaster=export){
    what <- match.arg(what, c("class", "entropy", "probability", "expected", "variation"), several.ok=TRUE)
    if (!missing('exportRaster')) {
        warning('argument "exportRaster" is deprecated; please use "export" instead.', call. = FALSE)
        export <- exportRaster
    }
    if(is.null(targetState)){
        targetState <- colnames(statesProb)
    } else {
        if("probability" %in% what){
            .checkStates(targetState, colnames(statesProb))
        } else {
            warning('Argument "targetState" will be ignored, as only useful when "probability" is required by "what" argument.')
        }
    }
    if('RasterLayer' %in% class(msk)){
        return( .mapRaster(target, statesProb, what, msk, midvals, targetState, spatial, export, path) )
    } else if('sf' %in% class(msk)){
        msk <- aoi(msk)
        return( .mapVector(target, statesProb, what, msk, midvals, targetState, spatial, export, path) )
    } else {
        stop('Please provide a valid "msk" argument (an object of class "RasterLayer" or "sf").')
    }
}

####
.classValue <- function(statesProb){
    classes <- apply(statesProb, 1, function(x){
        colnames(statesProb)[which.max(x)]
    })
    as.factor(classes)
}
####
.expectedValue <- function(statesProb, midvals) {
    if(length(midvals) != ncol(statesProb)){
        stop('Argument "midvals" must be a vector with length equal to the number of states of target node')
    }
    apply(statesProb, 1, function(x){x %*% midvals})
}
####
.variationValue <- function(statesProb, expect, midvals){
    uncertainty <- sapply(seq_along(expect), function(x) {
        s <- (midvals - expect[x])^2 * statesProb[x,]
        uncertainty <- sqrt(sum(s))
    })
    return(uncertainty / expect)
}
####
.entropyValue <- function(statesProb){
    apply(statesProb, 1, function(x){
        if(min(x) < 0 | sum(x) == 0){
            NA
        } else {
            x0 <- x[x > 0] / sum(x)
            -sum(log2(x0)*x0)
        }
    })
}
####
.probabilityValue <- function(statesProb, targetState){
    lapply(targetState, function(x) {statesProb[, x]} )
}
####
.writeOutputMaps <- function(rst, outFile, datatype){
    if(raster::canProcessInMemory(rst, 2)){
        raster::writeRaster(rst, outFile, overwrite=TRUE, datatype=datatype)
    } else {
        blocks <- raster::blockSize(rst, minblocks=2)
        out <- raster::writeStart(rst, outFile, overwrite=TRUE, datatype=datatype) ## open file to write
        for (i in 1:blocks$n){
            blockVals <- raster::getValues(rst, row=out$row[i], nrows=out$nrows[i]) ## values from rows to be written
            out <- raster::writeValues(out, blockVals, out$row[i]) ## populate the raster with values
        }
        out <- raster::writeStop(out)
    }
}

####           
.mapRaster <- function(target, statesProb, what, msk, midvals, targetState, spatial, exportRaster, path){
    if(exportRaster == TRUE){ # Leave like this, might be a character
        rFormat <- '.tif'
    } 
    if(is.character(exportRaster)){
        #match.arg(exportRaster, c('.asc','.sdat','.rst','.nc','.tif','.envi','.bil'))
        rFormat <- exportRaster
        exportRaster <- TRUE
    }
    if(spatial){
        msk_cells_ID <- msk
        msk_cells_ID[] <- seq_along(msk_cells_ID)
        msk_cells_ID <- raster::getValues(msk_cells_ID)[is.finite(raster::getValues(msk))]
        msk[] <- NA
    } else {
        msk_cells_ID <- seq_along(msk)
        msk_cells_ID <- msk_cells_ID[is.finite(raster::getValues(msk))]
    }
    whatList <- list()
    if('class' %in% what){
        Class <- .classValue(statesProb)
        if(spatial){
            Class <- match(Class, colnames(statesProb))
            msk[msk_cells_ID] <- Class
            keyLegend <- data.frame(colnames(statesProb), seq_along(colnames(statesProb)))
            names(keyLegend) <- c(target, 'cell_ID')
            if(exportRaster){
                .writeOutputMaps(msk, paste0(path, '/', target, '_Class', rFormat), 'INT2S')
                utils::write.csv(keyLegend, paste0(path, target, '_ClassKey.csv'), row.names = FALSE)
            } else {
                writeLines(paste0('Lookup table to interpret "', target, '" values:'))
                message(paste0(utils::capture.output(keyLegend), collapse = "\n"))
                # whatList$classLegend <- keyLegend
            }
            Class <- msk
        }
        whatList$Class <- Class
    }
    if('expected' %in% what | 'variation' %in% what){
        if(is.null(midvals)){
            warning('Could not calculate the expected value (and coefficient of variation) as either target ',
                    'node is categorical or mid-values for each states of target node were not provided.')
        } else {
            Expected <- .expectedValue(statesProb, midvals)
            if('variation' %in% what){
                Variation <- .variationValue(statesProb, Expected, midvals)
                if(spatial){ 
                    msk[msk_cells_ID] <- Variation
                    if(exportRaster){
                        .writeOutputMaps(msk, paste0(path, '/', target, '_CoeffVariation', rFormat), 'FLT4S')
                    } else {
                        Variation <- msk
                    }
                }
                whatList$CoeffVariation <- Variation			
            }
            if('expected' %in% what){
                if(spatial){ 
                    msk[msk_cells_ID] <- Expected
                    if(exportRaster){
                        raster::writeRaster(Expected, paste0(path, '/', target, '_ExpectedValue', rFormat), 
                                            datatype='FLT4S', overwrite=TRUE)
                    } else{
                        Expected <- msk
                    }
                }
                whatList$ExpectedValue <- Expected
            }
        }
    }
    if('entropy' %in% what){
        Entropy <- .entropyValue(statesProb)
        if(spatial){ 
            msk[msk_cells_ID] <- Entropy
            Entropy <- msk
            if(exportRaster){
                raster::writeRaster(Entropy, paste0(path, '/', target, '_ShanEntropy', rFormat), 
                                    datatype='FLT4S', overwrite=TRUE)
            }
        }
        whatList$Entropy <- Entropy
    }	
    if('probability' %in% what){
        Probability <- .probabilityValue(statesProb, targetState)
        if(spatial){
            Probability <- lapply(seq_along(Probability), function(x) {
                msk[msk_cells_ID] <- Probability[[x]]
                return(msk)
            })	
        }
        names(Probability) <- targetState
        whatList$Probability <- Probability
        if(spatial & exportRaster){
            lapply(seq_along(Probability), function(x) {
                raster::writeRaster(Probability[[x]], paste0(path, '/', target, '_Probability_', 
                                    targetState[x], rFormat), datatype='FLT4S', overwrite=TRUE)
            })
        }
    }
    if(exists('keyLegend')) whatList$classLegend <- keyLegend
    if(!spatial){
        xy <- raster::xyFromCell(msk, msk_cells_ID)
        whatList <- cbind(msk_cells_ID, xy, as.data.frame(whatList))
    }
    return(whatList)
}

####
.mapTab <- function(statesProb, what, midvals, targetState){
    whatList <- list()
    if('class' %in% what){
        Class <- .classValue(statesProb)
        whatList$Class <- Class
    }
    if('expected' %in% what | 'variation' %in% what){
        if(is.null(midvals)){
            warning('Could not calculate the expected value (nor coefficient of variation) as either target ',
                    'node seems to be categorical or mid-values for each states of target node were not provided.')
        } else {
            Expected <- .expectedValue(statesProb, midvals)
            if('variation' %in% what){
                Variation <- .variationValue(statesProb, Expected, midvals)
                whatList$CoeffVariation <- Variation			
            }
            if('expected' %in% what){
                whatList$ExpectedValue <- Expected
            }
        }
    }
    if('entropy' %in% what){
        Entropy <- .entropyValue(statesProb)
        whatList$Entropy <- Entropy
    }	
    if('probability' %in% what){
        Probability <- .probabilityValue(statesProb, targetState)
        names(Probability) <- targetState
        whatList$Probability <- Probability
    }
    return(as.data.frame(whatList))    
}

####
.mapVector <- function(target, statesProb, what, msk, midvals, targetState, spatial, exportShape, path){
    tab <- .mapTab(statesProb, what, midvals, targetState)
    if(!is.logical(exportShape)){ # Leave like this, might be character
        warning('Only ESRI shapefile are allowed as output format for vector data. "export" will be set to shapefile (.shp).')
        exportShape <- TRUE
    }
    rFormat <- "ESRI Shapefile" 
    if(spatial){
        tab <- sf::st_sf(data.frame(tab, msk)) 
        if(exportShape) {
            sf::write_sf(tab, dsn=paste0(path, '/', target, rFormat), driver=rFormat)
        }
    } else {
        tab <- cbind(FID=msk$FID, tab)
    }
    return(tab)
}

#' mapTarget
#' @title Make maps for target node
#'
#' @description This function creates the required spatial outputs for the target node.
#' @param target character. The node of interest to be modelled and mapped.
#' @param statesProb matrix. The probability matrix as returned by \code{\link{queryNet}} and \code{queryNetParallel}.
#' Columns are the \code{target} node states and rows each location considered from the area of interest.
#' @param what character. The required output, one or more of these values are valid:
#' \itemize{
#' \item{\code{"class"}} returns the relatively  most likely states.
#' \item{\code{"entropy"}} calculates the Shannon index and returns the entropy given the state probabilities.
#' \item{\code{"probability"}} returns an object for each state of the target node, with associated probability.
#' \item{\code{"expected"}} gives the expected value for the target node (see Details). Only valid for continuous target nodes. \code{midValues} argument must be provided.
#' \item{\code{"variation"}} returns the coefficient of variation, as a measure of uncertainty.
#' }
#' @param msk an object of class "RasterLayer". The reference raster to be used as mask. 
#' All model outputs will have the same resolution and same extent as this raster. All locations with no data (i.e. NA) cells 
#' in this raster will be ignored as well.
#' @param midvals vector of length equal to the number of states of the target node. Applies only if the target node is a continuous 
#' variable, in which case \code{midvals} must contain the mid values for each of the intervals 
#' @param targetState character. One or more states of interest from the target node. Applies only 
#' when argument \code{what} includes \code{'probability'}. Default is set to all states of the node.
#' @param spatial logical. Should the output be spatially explicit -i.e. a georeferenced raster? 
#' Default is TRUE, returning an object of class "RasterLayer". If FALSE, returns a data.frame 
#' with one row for each non NA cell in \code{msk} raster and in columns the output required by \code{mask} argument.
#' @param exportRaster Logical or character. Should the spatial output be exported to a raster file? 
#' Applies only if argument \code{spatial=TRUE}. When \code{exportRaster=TRUE}, rasters will be 
#' exported in .tif format. A character specifying another extension can be provided, in which case the 
#' raster will be exported in that format. Only formats listed by \link[raster]{writeFormats} are valid. 
#' @param path The directory to store the output files, when \code{exportRaster=TRUE}. 
#' Default is the working directory (\code{getwd()}).
#' @return A list of objects, one for each item required in \code{what} argument. If \code{spatial = TRUE} 
#' a list of rasters of class "RasterLayer" are returned, if FALSE a list of vectors with values 
#' associated to each non NA cell in msk raster (i.e. the vectorised raster). If argument \code{exportRaster} 
#' is specified, outputs are exported to files to the directory specified in \code{path}.
#' @details The expected value is calculated by summing the mid values of target node states weighted by their probability: \cr
#' \code{p1 * midVal_1 + p2 * midval_2 + ... + pn * midval_n}
#' @seealso \code{\link{bnspatial}}, \code{\link{aoi}}, \code{\link{queryNet}}
#' @examples
#' data(ConwyData)
#' network <- LandUseChange
#' target <- 'FinalLULC'
#' statesProb <- queryNet(network, target, evidence)
#' 
#' maps <- mapTarget(target, statesProb, msk=currentLU)
#' 
#' library(raster)
#' plot(maps$Class)
#' plot(maps$Entropy)
#' 
#' ## Create a probability surface for the "forest" state of target node "FinalLULC"
#' mp <- mapTarget('FinalLULC', statesProb, what='probability', targetState='forest', msk=currentLU)
#' plot(mp$Probability$forest)
#' 
#' @export
mapTarget <- function(target, statesProb, what=c("class", "entropy"), msk, midvals=NULL, targetState=colnames(statesProb), spatial=TRUE, exportRaster=FALSE, path=getwd()){
    .checkStates(targetState, colnames(statesProb))
    if(exportRaster == TRUE){
        rFormat <- '.tif'
    } else if (is.character(exportRaster)){
        #match.arg(exportRaster, c('.asc','.sdat','.rst','.nc','.tif','.envi','.bil'))
        rFormat <- exportRaster
    }
    what <- match.arg(what, c("class", "entropy", "probability", "expected", "variation"), several.ok = TRUE)
    if(spatial == TRUE){ 
        if(class(msk) == 'RasterLayer'){
            id <- msk
            id[] <- seq_along(id)
            id <- raster::getValues(id)[!is.na(raster::getValues(msk))]
            msk[] <- NA
        } else { 
            stop('Please provide a valid "msk" argument (an object of class "RasterLayer"), or set spatial=FALSE.')
        }
    }
    whatList <- list()
    if('class' %in% what){
        Class <- .classValue(statesProb)
        if(spatial == TRUE){
            Class <- match(Class, colnames(statesProb))
            msk[id] <- Class
            Class <- msk
            keyLegend <- data.frame(colnames(statesProb), seq_along(colnames(statesProb)))
            names(keyLegend) <- c(target, 'ID')
            if(exportRaster == TRUE){
                raster::writeRaster(Class, paste(path, '/', target, '_Class', rFormat, sep=''), overwrite=TRUE, datatype='INT2S')
                utils::write.csv(keyLegend, paste(path, target, '_ClassKey.csv', sep=''), row.names = FALSE)
            } else {
                writeLines(paste('Lookup table to interpret "', target, '" values:', sep=''))
                print(keyLegend)
            }
        }
        whatList$Class <- Class
    }
    if('expected' %in% what | 'variation' %in% what){
        if(is.null(midvals)){
            warning('Could not calculate the expected value (nor coefficient of variation) as either target node seems to be categorical or mid-values for each states of target node were not provided.')
        } else {
            Expected <- .expectedValue(statesProb, midvals)
            if('variation' %in% what){
                Variation <- .variationValue(statesProb, Expected, midvals)
                if(spatial == TRUE){ 
                    msk[id] <- Variation
                    Variation <- msk
                    if(exportRaster == TRUE){
                        raster::writeRaster(Variation, paste(path, '/', target, '_CoeffVariation', rFormat, sep=''), datatype='FLT4S', overwrite=TRUE)
                    }
                }
                whatList$CoeffVariation <- Variation			
            }
            if('expected' %in% what){
                if(spatial == TRUE){ 
                    msk[id] <- Expected
                    Expected <- msk
                    if(exportRaster == TRUE){
                        raster::writeRaster(Expected, paste(path, '/', target, '_ExpectedValue', rFormat, sep=''), datatype='FLT4S', overwrite=TRUE)
                    }
                }
                whatList$ExpectedValue <- Expected
            }
        }
    }
    if('entropy' %in% what){
        Entropy <- .entropyValue(statesProb)
        if(spatial == TRUE){ 
            msk[id] <- Entropy
            Entropy <- msk
            if(exportRaster == TRUE){
                raster::writeRaster(Entropy, paste(path, '/', target, '_Entropy', rFormat, sep=''), datatype='FLT4S', overwrite=TRUE)
            }
        }
        whatList$Entropy <- Entropy
    }	
    if('probability' %in% what){
        Probability <- .probabilityValue(statesProb, targetState)
        if(spatial == TRUE){
            Probability <- lapply(seq_along(Probability), function(x) {msk[id] <- Probability[[x]]; return(msk)})	
        }
        names(Probability) <- targetState
        whatList$Probability <- Probability
        if(spatial == TRUE & exportRaster == TRUE){
            lapply(seq_along(Probability), function(x) {
                raster::writeRaster(Probability[[x]], paste(path, '/', target, '_Probability_', targetState[x], rFormat, sep=''), datatype='FLT4S', overwrite=TRUE)
            })
        }
    }
    if(spatial == FALSE){
        xy <- raster::xyFromCell(msk, id)
        whatList <- cbind(id, xy, as.data.frame(whatList))
    }
    return(whatList)
}

.classValue <- function(statesProb){
    classes <- apply(statesProb, 1, function(x){
        colnames(statesProb)[which.max(x)]
    })
    as.factor(classes)
}

.expectedValue <- function(statesProb, midvals) {
    apply(statesProb, 1, function(x){x %*% midvals})
}

.variationValue <- function(statesProb, expect, midvals){
    uncertainty <- sapply(seq_along(expect), function(x) {
        s <- (midvals - expect[x])^2 * statesProb[x,]
        uncertainty <- sqrt(sum(s))
    })
    return(uncertainty / expect)
}

.entropyValue <- function(statesProb){
    apply(statesProb, 1, function(x){-x %*% log(x)} )
}

.probabilityValue <- function(statesProb, targetState){
    lapply(targetState, function(x) {statesProb[, x]} )
}

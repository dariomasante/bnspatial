#' dataDiscretize
#' @title Discretize data
#'
#' @description These functions discretize continuous input data into classes. Classes can be defined 
#' by the user or, if the user provides the number of expected classes, calculated 
#' from quantiles (default option) or by equal intervals.\cr
#' \code{dataDiscretize} processes a single variable at a time, provided as vector.
#' \code{bulkDiscretize} discretizes multiple input rasters, optionally by using parallel processing.
#' @rdname dataDiscretize
#' @aliases bulkDiscretize
#' @param data numeric vector. The continuous data to be discretized.
#' @param classBoundaries numeric vector or single integer. Interval boundaries to be used for data discretization. 
#' Outer values (minimum and maximum) required. \code{-Inf} or \code{Inf} are allowed, in which case 
#' data minimum and maximum will be used to evaluate the mid values of outer classes. Alternatively, a single integer to 
#' indicate the number of classes, to split by quantiles (default) or equal intervals.
#' @param classStates vector. The state labels to be assigned to the discretized data.
#' @param method character. What splitting method should be used? This argument is ignored if 
#' a vector of values is passed to \code{classBoundaries}.
#' \itemize{
#' \item{\code{quantile} splits data into quantiles (default). }
#' \item{\code{equal} splits data into equally sized intervals based on data minimum and maximum. }}
#' @param formattedLst A formatted list as returned by \code{\link{linkNode}} and \code{\link{linkMultiple}}
#' @param xy matrix. A matrix of spatial coordinates; first column is x (longitude), second column is y (latitude) of locations (in rows).
#' @param inparallel logical or integer. Should the function use parallel processing facilities? Default is FALSE: a single process will be launched. If TRUE, all cores/processors but one will be used.
#' Alternatively, an integer can be provided to dictate the number of cores/processors to be used. 
#' @return \code{dataDiscretize} returns a named list of 4 vectors: 
#' \itemize{
#' \item{\code{$discreteData}}{the discretized data, labels are applied accordingly if \code{classStates} argument is provided }
#' \item{\code{$classBoundaries}}{the class boundaries, i.e. values splitting the classes }
#' \item{\code{$midValues}}{the mid point for each class (the mean of its lower and upper boundaries) }
#' \item{\code{$classStates}}{the labels assigne to each class }
#' }
#' \code{bulkDataDiscretize} returns a matrix: in columns each node associated to input spatial data, 
#' in rows their discretized values at coordinates specified by argument \code{xy}.
#' @examples
#' s <- runif(30)
#'
#' # Split by user defined values. Values out of boundaries are set to NA:
#' dataDiscretize(s, classBoundaries = c(0.2, 0.5, 0.8)) 
#'
#' # Split by quantiles (default):
#' dataDiscretize(s, classStates = c('a', 'b', 'c'))
#'
#' # Split by equal intervals:
#' dataDiscretize(s, classStates = c('a', 'b', 'c'), method = "equal")
#'
#' # When -Inf and Inf are provided as external boundaries, $midValues of outer classes
#' # are calculated on the minimum and maximum values:
#' dataDiscretize(s, classBoundaries=c(0, 0.5, 1), classStates=c("first", "second"))[c(2,3)]
#' dataDiscretize(s, classBoundaries=c(-Inf, 0.5, Inf), classStates=c("first", "second"))[c(2,3)]
#' 
#' ## Discretize multiple spatial data by location
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
#' 
#' # Link multiple spatial data to the network nodes and discretize
#' spDataLst <- linkMultiple(spatialData, network, LUclasses, verbose = FALSE)
#' coord <- aoi(ConwyLU, xy=TRUE)
#' head( bulkDiscretize(spDataLst, coord) )
#' @export
dataDiscretize <- function(data, classBoundaries=NULL, classStates=NULL, method="quantile"){
    match.arg(method, c("quantile", "equal"))
    mn <- min(data, na.rm=TRUE)
    mx <- max(data, na.rm=TRUE)
    
    classBoundaries <- .makeClassBoundaries(data=data, classBoundaries=classBoundaries, 
                                            classStates=classStates, method=method, mn=mn, mx=mx)
    
    minimum <- classBoundaries[1]
    maximum <- classBoundaries[length(classBoundaries)]
    discreteData <- findInterval(data, classBoundaries, rightmost.closed=TRUE)
    #discreteData[which(data == maximum)] <- length(classBoundaries) - 1  #Fix max value exclusion from findInterval
    discreteData[discreteData == 0 | discreteData == length(classBoundaries)] <- NA  #Remove extra classes, out of boundaries
    
    breaks <- classBoundaries
    minimum <- ifelse(minimum == -Inf, mn, minimum)
    maximum <- ifelse(maximum == Inf, mx, maximum)
    breaks[c(1, length(breaks))] <- c(minimum, maximum)
    midValues <- sapply(1:(length(breaks)-1), function(x) {(breaks[x] + breaks[x+1])/2})
    if(!is.null(classStates)){
        discreteData <- classStates[discreteData]
    } else {
        classStates <- as.character(seq_along(midValues))
    }
    return(list(discreteData=discreteData, classBoundaries=classBoundaries, 
                midValues=midValues, classStates=classStates))
}

.makeClassBoundaries <- function(data, classBoundaries, classStates, method, mn, mx){
    if(is.null(classBoundaries)){
        if(is.null(classStates)){ 
            stop('Must provide either "classBoundaries", "classStates" or both') 
        } else if(length(classStates) < 2){
            stop('"classStates" must be a vector of length greater than 1')
        } else {
            classBoundaries <- length(classStates)
        }
    }
    if(length(classBoundaries) == 1){
        if(!is.null(classStates) & classBoundaries != length(classStates)){
            stop('Number of bins must match the number of states')
        }
        if(classBoundaries < 2 | abs(classBoundaries - round(classBoundaries)) > 0 ){
            stop('"classBoundaries" must be an integer greater than 1, 
                 or a vector of values to be used as class boundaries')
        }
        if(method == "quantile"){
            classBoundaries <- stats::quantile(data, probs=cumsum(rep(1/classBoundaries, classBoundaries-1)), 
                                               na.rm=TRUE, names = FALSE)
            classBoundaries <- c(mn, classBoundaries, mx)
            if(any(duplicated(classBoundaries))){
                stop('Non unique quantile separators (a single value may cover a substantial fraction of the data).',
                     ' Please specify a vector of class boundaries instead.')
            }
        } 
        if(method == "equal"){
            intervalSize <- (mx - mn) / classBoundaries
            classBoundaries <- mn + cumsum(rep(intervalSize, classBoundaries-1))
            classBoundaries <- c(mn, classBoundaries, mx)
        }
        } else if(!is.null(classStates)){
            if(!identical(classStates, unique(classStates))){
                stop('Non unique states defined')
            }
            if((length(classBoundaries)-1) != length(classStates)){
                stop('Number of bins must match the number of states')
            }
        }
    ## perform some more checks  
    if(!identical(classBoundaries, sort(classBoundaries))){
        stop('"classBoundaries" must be provided from lowest to highest')
    }
    if(!identical(classBoundaries, unique(classBoundaries))){
        warning('Non unique values provided in "classBoundaries"')
    }
    cb <- classBoundaries[-c(1, length(classBoundaries))]
    if(any(cb < mn | cb > mx)){
        cb <- classStates[which(cb < mn | cb > mx)]
        warning('One or more classes (i.e. node states) fall entirely out of input data range. Check "classBoundaries" if in doubt:  \n', 
                paste(cb, collapse=' ; '))
    }
    if(length(cb) == 0 & any(classBoundaries[2] < mn | classBoundaries[1] > mx)){
        stop('All classes (i.e. node states) fall out of input data range. Check "classBoundaries" or the input data.')
    }
    return(classBoundaries)
    }

#' @rdname dataDiscretize
#' @export
bulkDiscretize <- function(formattedLst, xy, inparallel=FALSE){
    inparallel <- .inParallel(inparallel)
    is.sf <- 'sf' %in% class(formattedLst$SpatialData)
    if(is.sf) formattedLst['SpatialData'] <- NULL
    if(inparallel == 1){
        lst <- lapply(names(formattedLst), function(x){
            if(is.sf){
                ex <- formattedLst[[x]]$SpatialData[xy]
            } else {
                layer <- formattedLst[[x]]$SpatialData
                ex <- extractByMask(layer, msk=xy)
            }
            if(formattedLst[[x]]$Categorical == TRUE){
                formattedLst[[x]]$States[match(ex, formattedLst[[x]]$ClassBoundaries)]
            } else {
                dataDiscretize(ex, formattedLst[[x]]$ClassBoundaries, formattedLst[[x]]$States)[[1]]
            }
        })
        df <- matrix(unlist(lst), ncol=length(lst))
    } else {
        if(is.sf){
            splittedData <- split(xy, ceiling(seq_along(xy)/inparallel))
        } else {
            splittedData <- split(as.data.frame(xy), (seq(nrow(xy))-1) %/% (nrow(xy) / inparallel ) )
        }
        if(exists('tokenToHaltChildrenFromParallelProc', envir=parent.frame()) == FALSE){
            clst <- parallel::makeCluster(inparallel)
            doParallel::registerDoParallel(clst)
        }
        i <- NULL # Trick to remove NOTE from R package release check 
        o <- foreach::foreach(i = seq_along(splittedData), .combine=rbind, .packages="raster")
        df <- foreach::"%dopar%"(o, {
            lst <- lapply(names(formattedLst), function(x){
                if(is.sf){
                    ex <- formattedLst[[x]]$SpatialData[splittedData[[i]] ]
                } else {
                    layer <- formattedLst[[x]]$SpatialData
                    ex <- extractByMask(layer, msk=as.matrix(splittedData[[i]]))
                }
                if(formattedLst[[x]]$Categorical == TRUE){
                    formattedLst[[x]]$States[match(ex, formattedLst[[x]]$ClassBoundaries)]
                } else {
                    dataDiscretize(ex, formattedLst[[x]]$ClassBoundaries, formattedLst[[x]]$States)[[1]]
                }
            })
            matrix(unlist(lst), ncol=length(lst))
        })
        if(exists('tokenToHaltChildrenFromParallelProc', envir=parent.frame()) == FALSE){
            parallel::stopCluster(clst); gc()
        }
    }
    colnames(df) <- names(formattedLst)
    return(df)
}

###
.inParallel <- function(inparallel){
    if(is.logical(inparallel)){
        nc <- ifelse(inparallel == TRUE, parallel::detectCores()-1, 1)
    } else {
        #nc <- min(parallel::detectCores(), inparallel)
        nc <- inparallel
    }
    return(nc)
}

#' @name ConwyData
#' @title Land use change data
#'
#' @description Data derived from the Conwy catchment in North Wales (UK), widely modified
#' for demonstration purposes. Once loaded, the data consist of several objects:
#' \itemize{
#'   \item LandUseChange An object of class \href{https://cran.r-project.org/web/packages/gRain/index.html}{\code{grain}}. 
#'   The Bayesian network.
#'   \item currentLU An object of class \href{https://cran.r-project.org/web/packages/raster/index.html}{\code{RasterLayer}}. 
#'   A simplified version of the current land use map from the Conwy catchment (Wales, UK). It includes three classes: arable (raster value 3), forest (2), other (1).
#'   \item slope An object of class RasterLayer. A raster of slope derived from a digital elevation model at 50 meters resolution, units are degrees.
#'   \item status An object of class RasterLayer. The land ownership type (dummy data), divided into three possible classes: public (raster value 4), private (3), protected (1).
#'   \item evidence A matrix. The collection of available spatial data (see above) as extracted from each location (i.e. cell) 
#'   in the catchment, where the latter is represented by the raster object \code{currentLU}. Each value from the spatial data was 
#'   discretized through \code{\link{dataDiscretize}} or \code{\link{bulkDiscretize}} functions, then assigned to the corresponding 
#'   state from the Bayesian network (LandUseChange).
#'   \item LUclasses A list with the classification of input spatial data (its corresponding states and values). The list is formatted accordingly to 
#'   bnspatial functions requirement and as returned by functions \code{\link{importClasses}} and \code{\link{setClasses}}.
#' }
#' @docType data
#' @usage data(ConwyData)
#' @format A dataset in native \code{RData} format.
#' @examples
#' library(bnspatial)
#' data(ConwyData)
#' ls()
#' 
#' ## The network nodes and states
#' LandUseChange$universe$levels
#' 
#' ## Lookup list relating raster values and network nodes
#' LUclasses
#' 
#' ## Table of evidence extracted from input spatial data
#' head(evidence)
#' 
#' ## The input spatial data
#' par(mfrow=c(2,2))
#' raster::plot(currentLU)
#' raster::plot(slope)
#' raster::plot(status)
NULL

#' @name LandUseChange
#' @title Bayesian network
#' @description Bayesian network built for demonstration purposes, inspired by works like Celio et al (2014).
#' @docType data
NULL

#' @name slope
#' @title Slope raster for Conwy catchment.
#' @description An object of class RasterLayer. A raster of slope derived from a digital elevation model at 50 meters resolution, units are degrees.
#' @docType data
NULL

#' @name status
#' @title Ownership/legal status raster for Conwy catchment (dummy data).
#' @description An object of class RasterLayer. The land ownership type (dummy data), divided into three possible classes: public (raster value 4), private (3), protected (1).
#' @docType data
NULL

#' @name evidence
#' @title Evidence from extracted spatial inputs.
#' @description A matrix. The collection of available spatial data (see above) as extracted from each location (i.e. cell) 
#'   in the catchment, where the latter is represented by the raster object \code{currentLU}. Each value from the spatial data was 
#'   discretized through \code{\link{dataDiscretize}} or \code{\link{bulkDiscretize}} functions, then assigned to the corresponding 
#'   state from the Bayesian network (LandUseChange).
#' @docType data
NULL

#' @name currentLU
#' @title Current land use map (heavily aggregated)
#' @description An object of class \href{https://cran.r-project.org/web/packages/raster/index.html}{\code{RasterLayer}}. 
#' A simplified version of the current land use map from the Conwy catchment (Wales, UK). 
#' It includes three classes: arable (raster value 3), forest (2), other (1).
#' @docType data
NULL

#' @name LUclasses
#' @title Lookup list, linking input spatial data and the Bayesian network
#' @description A list with the classification of input spatial data (its corresponding states and values). 
#' The list is formatted accordingly to bnspatial functions requirement and as returned by 
#' functions \code{\link{importClasses}} and \code{\link{setClasses}}.
#' @docType data
NULL
 
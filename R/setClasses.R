#' @name setClasses
#' @title Set classes or intervals
#' 
#' @description Functions \code{setClasses} and \code{importClasses} return a formatted list from given arguments, to be used for the integration and error checking of Bayesian network and input spatial variables.
#' For \code{setClasses} a vector with node names and a list of vectors for both states of nodes and (optional) their boundaries in the spatial data 
#' must be provided, in the right order.
#' For \code{importClasses} a formatted text file must be provided (see Details).
#' @aliases importClasses
#' @rdname setClasses
#' @param classFile	character. A text file where for each input variable associated to a node (see Details) three lines are specified as follows: 
#' the first one indicates the node name, as in the Bayesian network; the second indicates the states associated with such node, as they are in the Bayesian network (note that underscores are not allowed);
#' the third one contains the values associated to each state in the spatial data (for discrete variables) or the class boundaries dividing the states (for continuous variables), including minimum and maximum.
#' @param nodes character. The nodes known and available as spatial data.
#' @param states A list of characters. The states associated to each of the nodes (order must match \code{nodes} names)
#' @param classBoundaries A list of numeric. The boundary values splitting the nodes into their corresponding states. They must be sorted in ascending order. For nominal categorical variables, \code{classBoundaries} must be the unique raster values associated to node states. 
#' @param wr The full path to the file to be written. Default is set to NULL, otherwise it writes the formatted list returned by \code{setClasses} to the specified path. Suggested file format is .txt, albeit not mandatory.
#' @return A formatted list, specifying states break values for continuous nodes and integer values for categorical nodes.
#' @details As a reference for the text file format required by \code{importClasses}, for each node of the network:\cr
#' \strong{First line}: the node name.\cr
#' \strong{Second line}: the node states, comma separated (spaces allowed).\cr
#' \strong{Third line}: interval values from the spatial data associated to the states (integer values for discrete data; interval boundaries, including endpoints, for continuous data). The same exact order as node states is required.\cr
#' 
#' For example:
#' 
#' \code{CurrentLULC} \cr
#' \code{forest,other,arable} \cr
#' \code{2, 1, 3} \cr
#' \code{Slope} \cr
#' \code{flat, moderate, steep} \cr
#' \code{-Inf, 1, 7, Inf} \cr
#' \code{LegalStatus} \cr
#' \code{public, private, protected} \cr
#' \code{4, 3, 1 }
#' 
#' It is possible to write the formatted file automatically using \code{setClasses}, by setting argument \code{w} as path to the text file to be created.
#' @seealso \code{\link{dataDiscretize}}
#' @examples
#' ## Load classes from external formatted text file
#' # Not run: importClasses('LUclasses.txt')
#' raw = system.file("extdata/LUclasses.txt", package = "bnspatial")
#' importClasses(raw)
#' 
#' ## Same as:
#'
#' setClasses(c('Slope', 'CurrentLULC', 'LegalStatus'), list(c('flat', 'moderate', 'steep'),
#' c('forest', 'arable', 'other'), c('public', 'private', 'protected')),
#' list(c(-Inf, 0, 5, Inf), c(2, 3, 1), (c(4, 3, 1))))
#' 
#' @export
setClasses <- function(nodes, states, classBoundaries, wr=NULL){
    if(length(nodes) != length(states) | length(nodes) != length(classBoundaries)){
        stop('Number of nodes not matching the length of "states" list and/or that of classBoundaries provided.')
    }
    lst <- vector('list', length = length(nodes))
    names(lst) <- nodes
    for(i in seq_along(names(lst)) ){
        if(!identical(states[[i]], unique(states[[i]]))){
            print(states[[i]])
            stop('Non unique node state defined')
        }		
        lst[[i]]$States <- states[[i]]
        if(!identical(classBoundaries[[i]], unique(classBoundaries[[i]]))){
            print(classBoundaries[[i]])
            stop('Non unique "classBoundaries" defined')
        }
        if((length(classBoundaries[[i]]) - length(states[[i]])) %in% c(0, 1)){
            categorical <- ifelse(length(classBoundaries[[i]]) == length(states[[i]]), TRUE, FALSE)
        } else {
            stop('Number of classes not matching the number of states. Also, for non categorical data 
                 minimum and maximum boundaries must be set (-Inf and Inf allowed).')
        }
        lst[[i]]$ClassBoundaries <- classBoundaries[[i]]
        lst[[i]]$Categorical <- categorical
        if( identical(classBoundaries[[i]], sort(classBoundaries[[i]])) == FALSE & categorical == FALSE){
            print(classBoundaries[[i]])
            stop('"classBoundaries" for non categorical data must be sorted from lowest to highest')
        }
        }
    if(!is.null(wr)){
        .makeClassFile(wr, lst)
    }
    return(lst)
}

#' @rdname setClasses
#' @export
importClasses <- function(classFile){
    if(!is.character(classFile) | length(classFile) != 1){
        stop('Argument "classFile" must be a character of length one')
    }
    nodes <- vector()
    states <- list()
    classBoundaries <- list()
    classTxt <- scan(classFile, character(0), sep = "\n")
    for(i in seq(1, length(classTxt), by=3)){
        node <- classTxt[i]
        nodes <- c(nodes, node)
        state <- gsub(" ", "", unlist(strsplit(classTxt[i+1], ",")))
        states[[node]] <- state
        class <- as.numeric(gsub(" ", "", unlist(strsplit(classTxt[i+2], ","))))
        classBoundaries[[node]] <- class
    }
    setClasses(nodes=nodes, states=states, classBoundaries=classBoundaries)
}

.makeClassFile <- function(f, lst){
    cat("", file=f)
    for(i in seq_along(lst)){
        nm <- names(lst[i])
        cat(nm, paste(lst[[i]][[1]], collapse=","), paste(lst[[i]][[2]], collapse=","), sep="\n", file=f, append=TRUE)
    }
}

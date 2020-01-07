#' @name loadNetwork
#' @title Load a Bayesian network
#' @description This function loads the Bayesian network from a native gRain object of class \code{grain} or an external 
#' file with extension \emph{.net} (as provided by external softwares \href{http://www.hugin.com/}{Hugin} or \href{http://www.bayesfusion.com/}{GeNIe}), 
#' optionally compiling the network.
#' @param network The Bayesian network. An object of class \code{grain}, or a character (the path to the \emph{.net} file to be loaded)
#' @param target character. The node of interest to be modelled and mapped.
#' @note Under current release, this function wraps a set of hidden functions copied in block from the \href{https://cran.r-project.org/package=gRain}{gRain} package, as current CRAN policy
#' discourages accessing hidden functions with the ":::" operator. These functions will be progressively substituted by bnspatial native ones.
#' @return An object of class \code{grain}. The Bayesian network. If \code{target} argument is provided the network is compiled for a faster querying .
#' @details Bayesian networks built with the package \href{https://cran.r-project.org/package=bnlearn}{bnlearn} can be imported with the function \code{bnlearn::as.grain}, which converts them into \code{grain} objects.\cr
#' \emph{.net} file format as provided from Netica 5.24 currently does not correspond to a valid Hugin .net file.\cr
#' Argument \code{target} has default set to NULL, but if provided the network will be compiled for faster querying.
#' @examples
#' ## Load from external file (.net format)
#' raw = system.file("extdata/LandUseChange.net", package = "bnspatial")
#' loadNetwork(raw)
#' 
#' ## Compile using target node
#' loadNetwork(raw, 'FinalLULC')
#' @export
loadNetwork <- function(network, target=NULL){
    if (all(class(network) != "grain")){  # Check if a gRain network is loaded
        if(class(network) == 'character' & length(network) == 1 & grepl('.net', network)){
            network <- .loadNet(network) # If not load Bayesian network from file path
        } else {
            stop('Input argument "network" must be a .net file from an external software such as ', 
                 'Hugin or GeNIe, or an object of class "grain" from the gRain package')
        }
    }
    if(!is.null(target)){
        .checkNames(network, target)
        network <- gRbase::compile(network, root=target, propagate=TRUE) # Compile network to speed up queries
    }
    return(network)
}


## This set of functions were copied in block from the gRain package, as current CRAN policy
## discourages accessing hidden functions with `:::` operator. The problem is that gRain::loadHuginNetwork 
## does not load .net files from the GeNIE software correctly and a simple tweak in the hidden functions 
## is able to fix that. These functions will be progressively substituted by bnspatial native ones.

.loadNet = function(file, description = rev(unlist(strsplit(file, "/")))[1], details = 0) {
    xxx <- .readHugin(file, details)
    
    xxx$nodeList <- lapply(xxx$nodeList, .fixLines) ## This line fixes Genie quirk when saving .net files
    
    yyy <- .transformHuginNet2internal(xxx)
    universe <- .asUniverse(yyy)
    plist <- lapply(yyy$potentialList, .hpot2cptable, universe)
    value <- gRain::grain(gRain::compileCPT(plist))
    return(value)
}

.fixLines <- function(nodeLines) {
  s <- which(grepl('states = ', nodeLines))
  while(grepl( ');', nodeLines[s]) == FALSE){
    nodeLines[s] <- paste(nodeLines[s], nodeLines[s+1], sep='')
    nodeLines <- nodeLines[-(s+1)]
  }
  return(nodeLines)
}

.asUniverse <- function (from) 
{
    ccshort <- sapply(from$nodeList, function(x) x$nodeVar)
    ccnames <- sapply(from$nodeList, function(x) x$nodeLabel)
    cclabels <- lapply(from$nodeList, function(x) x$nodeStates)
    names(cclabels) <- ccnames
    di <- c(lapply(cclabels, length), recursive = TRUE)
    list(nodes = ccnames, short = ccshort, levels = cclabels, 
         nlev = di)
}

.hpot2cptable <- function (cpot, universe) 
{
    idx <- match(c(cpot[c("nodeVar", "parentVar")], recursive = TRUE), 
                 universe$short)
    vpa <- universe$nodes[idx]
    v <- vpa[1]
    gRain::cptable(vpa, values = cpot$potential, levels = universe$levels[[v]])
}

.readHugin <- function (file, details = 0) 
{
    nodeCount <- 0
    con <- file(file, "rb")
    repeat {
        cline <- readLines(con, n = 1)
        if (!length(cline)) 
            (break)()
        if (.hasToken("node", cline)) 
            nodeCount <- nodeCount + 1
    }
    close(con)
    nodeList <- potentialList <- as.list(rep(NA, nodeCount))
    con <- file(file, "rb")
    currNode <- currPotential <- 1
    state <- "start"
    repeat {
        cline <- readLines(con, n = 1)
        if (!length(cline)) 
            (break)()
        switch(state, start = {
            if (.hasToken("net", cline)) {
                state = "net"
                wline <- cline
            }
        }, net = {
            wline <- c(wline, cline)
            if (.hasToken("}", cline)) {
                state = "run1"
            }
        }, run1 = {
            if (.hasToken("node", cline)) {
                state = "node"
            } else {
                if (.hasToken("potential", cline)) {
                    state = "potential"
                }
            }
            wline <- cline
        }, node = {
            wline <- c(wline, cline)
            if (.hasToken("}", cline)) {
                state = "run1"
                nodeList[[currNode]] <- wline
                currNode <- currNode + 1
            }
        }, potential = {
            wline <- c(wline, cline)
            if (.hasToken("}", cline)) {
                state = "run1"
                potentialList[[currPotential]] <- wline
                currPotential <- currPotential + 1
            }
        })
    }
    close(con)
    nodeList <- nodeList[!sapply(lapply(nodeList, is.na), all)]
    potentialList <- potentialList[!sapply(lapply(potentialList, 
                                                  is.na), all)]
    value <- structure(list(nodeList = nodeList, potentialList = potentialList))
    return(value)
}

.hasToken <- function (token, cline) 
{
    cline <- gsub("^ +", "", cline)
    a <- unlist(strsplit(cline, " "))[1]
    if (!is.na(a)) 
        a == token
    else FALSE
}

.transformHuginNet2internal <- function (x) 
{
    nodeList2 <- lapply(x$nodeList, .getNodeSpec)
    potentialList2 <- lapply(x$potentialList, .getPotentialSpec)
    nl <- .makeNodeNamesUnique(nodeList2)
    repeat {
        if (length(nl$nonunique) == 0) 
            (break)()
        nl <- .makeNodeNamesUnique(nl$nodeList)
    }
    nodeList2 <- nl$nodeList
    value <- structure(list(nodeList = nodeList2, potentialList = potentialList2))
    class(value) <- "huginnet"
    return(value)
}

.makeNodeNamesUnique <- function (nodeList2) 
{
    nl <- t(sapply(nodeList2, function(d) unlist(d[1:2])))
    nonunique <- names(which(table(nl[, 2]) > 1))
    if (length(nonunique)) {
        cat("Label(s): {", nonunique, "} appears mode than once in NET file\n")
        for (i in 1:length(nonunique)) {
            cnu <- nonunique[i]
            idx <- which(cnu == nl[, 2])
            for (j in idx) {
                a <- nodeList2[[j]]$nodeVar
                cat("  Replacing label", cnu, " with node name", 
                    a, "\n")
                nodeList2[[j]]$nodeLabel <- a
            }
        }
    }
    return(list(nodeList = nodeList2, nonunique = nonunique))
}

.getNodeSpec <- function (nodeSpec) 
{
    tmp <- nodeSpec[which(as.logical(lapply(nodeSpec, function(d) grep("node", d))))]
    nodeVar <- gsub("node +", "", tmp)[1]
    nodeVar <- gsub(" +", "", nodeVar)
    tmp <- nodeSpec[which(as.logical(lapply(nodeSpec, function(d) grep("label", d))))]
    nodeLabel <- gsub(" +label += +", "", tmp)
    nodeLabel <- gsub(";", "", nodeLabel)
    nodeLabel <- gsub("\"", "", nodeLabel)
    nodeLabel <- gsub(" +", " ", nodeLabel)
    if (length(nodeLabel) && nchar(nodeLabel) > 0) {
        nodeLabel <- .toCamel(nodeLabel)
        nl <- gsub("[^[:alnum:]]", "", nodeLabel)
        nodeLabel <- gsub("[^[:alnum:]|\\.]", "", nodeLabel)
        base <- as.character(0:9)
        if (gRbase::subsetof(unlist(strsplit(nl, "")), base)) {
            nodeLabel <- paste("X", nodeLabel, sep = "")
        }
    }
    else {
        nodeLabel <- nodeVar
    }
    tmp <- nodeSpec[which(as.logical(lapply(nodeSpec, function(d) grep("states", d))))]
    nodeStates <- gsub(" +states += +", "", tmp)
    nodeStates <- gsub("[\\(,\\);]", "", nodeStates)
    nodeStates <- unlist(strsplit(nodeStates, "\\\""))
    nodeStates <- sapply(nodeStates, function(d) gsub("^ +", 
                                                      "", d))
    nodeStates <- nodeStates[sapply(nodeStates, nchar) > 0]
    nodeStates <- sapply(nodeStates, .toCamel)
    nodeStates <- gsub(" +", ".", nodeStates)
    names(nodeStates) <- NULL
    value <- list(nodeVar = nodeVar, nodeLabel = nodeLabel, nodeStates = nodeStates)
    value
}

.toCamel <- function (s) 
{
    s <- gsub(" +", " ", s)
    s <- unlist(strsplit(s, " "))
    paste(c(s[1], sapply(s[-1], .capWords)), collapse = "")
}

.capWords <- function (s, strict = FALSE) 
{
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
        s <- substring(s, 2)
        if (strict) 
            tolower(s)
        else s
    }, sep = "", collapse = " ")
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

.getPotentialSpec <- function (potSpec) 
{
    tmp <- potSpec[which(as.logical(lapply(potSpec, function(d) grep("potential", d))))]
    tmp <- gsub("potential +", "", tmp)
    tmp <- gsub("[\\(,\\),|]", "", tmp)
    tmp <- gsub(" +", " ", tmp)
    tmp <- unlist(strsplit(tmp, " "))
    tmp <- tmp[sapply(tmp, nchar) > 0]
    nodeVar <- tmp[1]
    parentVar <- tmp[-1]
    sss <- paste(potSpec, collapse = "")
    sss2 <- gsub("^.*data[[:space:]]*=([^;]*);(.*)", "\\1", sss)
    sss3 <- gsub("\\)[^\\)]*\\(", ") (", sss2)
    sss4 <- gsub("[\\(,\\),\\}]", "", sss3)
    sss5 <- gsub("^[[:space:]]*", "", sss4)
    sss6 <- gsub("[[:space:]]$*", "", sss5)
    sss7 <- strsplit(sss6, " +")[[1]]
    pot <- as.numeric(sss7)
    value <- list(nodeVar = nodeVar, parentVar = rev(parentVar), 
                  potential = pot)
    value
}
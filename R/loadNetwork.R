#' @name loadNetwork
#' @title Load a Bayesian network
#' @description This function loads the Bayesian network from a native gRain object of class \code{grain} or an external 
#' file with extension \emph{.net} or \emph{.xdsl} (as provided by external softwares \href{http://www.hugin.com/}{Hugin} or \href{http://www.bayesfusion.com/}{GeNIe}), 
#' optionally compiling the network.
#' @param network The Bayesian network. An object of class \code{grain}, or a character (the path to the \emph{.net} or \emph{.xdsl} file to be loaded)
#' @param target character. The node of interest to be modelled and mapped.
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
        if(class(network) == 'character' & length(network) == 1 & any(endsWith(network, c('.net','.xdsl')))){
            network <- .loadNet(network) # If not load Bayesian network from file path
        } else {
            stop('Input argument "network" must be a .net or .xdsl file from an external software such as ', 
                 'Hugin or GeNIe, or an object of class "grain" from the gRain package')
        }
    }
    # if(!is.null(target)){
    #     .checkNames(network, target)
    #     network <- gRbase::compile(network, root=target, propagate=TRUE) # Compile network to speed up queries
    # }
    return(network)
}

# Function to load external .dne files (Netica)
.extract_dne <- function(lines, pattern, nodes=FALSE){
    presence <- grep(pattern, lines)
    v <- rep(NA, length(lines))
    nms <- sapply(strsplit(lines[presence], ifelse(nodes,' ','=')), '[[', 2)
    if(nodes){
        v <- c(rep(NA, presence[1]-1), nms[cumsum(seq_along(v) %in% presence)])
    } else {
        v[presence] <- gsub(';', '', trimws(gsub("[()]", "", nms)))
    }
    return(v)
}

# CHECK what happens if no decimal is provided (i.e. prob set to zero/one) 
.read_dne <- function(dne){
    f <- readLines(dne)
    nodes <- .extract_dne(f, "node (.*?) \\{", TRUE)
    sts <- .extract_dne(f, "states = (.*?);") # get all statuses
    w_sts <- which(!is.na(sts))
    prt <- .extract_dne(f, "parents = (.*?);")
    chc <- .extract_dne(f, "chance = (.*?);")
    knd <- .extract_dne(f, "kind = (.*?);")
    dsc <- .extract_dne(f, "discrete = (.*?);")
    tb <- data.frame(nodes=nodes[w_sts], 
                     status=sts[w_sts], 
                     parents=prt[!is.na(prt)][1:length(w_sts)],
                     chance=chc[!is.na(chc)][1:length(w_sts)],
                     kind=knd[!is.na(knd)][1:length(w_sts)],
                     discrete=dsc[!is.na(dsc)][1:length(w_sts)],
                     stringsAsFactors=FALSE)
    if(any(tb$chance != 'CHANCE')) {
        stop('Check node types of ', paste(tb$nodes[tb$chance != 'CHANCE'],collapse=','),
             ' from Netica file: only chance node types are allowed.')
    }
    if(any(tb$kind != 'NATURE')) {
        stop('Check node types of ',paste(tb$nodes[tb$kind != 'NATURE'],collapse=','),
        ' from Netica file, only nature node types are allowed.')
    }
    if(any(tb$discrete != 'TRUE')) {
        stop('Check node types of ',paste(tb$nodes[tb$discrete != 'TRUE'],collapse=','),
        ' from Netica file, only discrete nodes are allowed.')
        names(lst) <- tb$nodes
    }
    lst <- vector(mode = "list", length = nrow(tb))
    names(lst) <- nodes[w_sts]
    spb <- str_detect(f, "probs =") # get start of cpt
    stt <- str_detect(f, "title =") # get end of cpt
    ctrl <- FALSE
    for(i in 1:length(spb)){ # isolate between the two 
        if(stt[i]){ # Keep order of if!!
            ctrl <- FALSE
        }
        if(ctrl){
            lst[[nodes[i]]] <- c(lst[[nodes[i]]], trimws(gsub('\t\t','',f[i])))
        }
        if(spb[i]){
            ctrl <- TRUE
        }
    }
    states <- strsplit(tb$status, ', ')
    parents <- strsplit(tb$parents, ', ')
    # na.omit(as.numeric(unlist(strsplit(lst[[i]],"[^[:digit:].]"))))
    lst <- lapply(1:length(lst), function(i){
        # if(!is.null(lst[[i]])){ # case for node different from chance/nature
        a <- unlist(strsplit(lst[[i]], ','))
        probs <- as.numeric(regmatches(a,regexpr("(^|\\d+)\\.\\d+",a)))
        gRain::cptable(c(tb$nodes[i], parents[[i]]), states[[i]], matrix(probs))
        # }
    })
    names(lst) <- tb$nodes
    return(lst)
}

# Function to load external .xdsl file (Genie)
.read_xdsl <- function(xdsl){
    x <- xml2::read_xml(xdsl)    
    recs <- xml2::xml_find_all(x, "//cpt")
    nodes <- xml2::xml_attr(recs, "id")
    probs <- lapply(strsplit(xml2::xml_text(xml2::xml_find_all(x, "//probabilities")), ' '), as.numeric)
    lst <- lapply(1:length(recs), function(i){
        states <- xml2::xml_attr(xml2::xml_find_all(recs[i], './/state'), 'id')
        parents <- unlist(strsplit(xml2::xml_text(xml2::xml_find_all(recs[i], './/parents')), ' '))
        gRain::cptable(c(nodes[i], rev(parents)), states, probs[[i]])
    })
    names(lst) <- nodes
    return(lst)
}

## This set of functions were copied in block from the gRain package, as current CRAN policy
## discourages accessing hidden functions with `:::` operator. The problem is that gRain::loadHuginNetwork 
## does not load .net files from the GeNIE software correctly and a simple tweak in the hidden functions 
## is able to fix that. These functions will be progressively substituted by bnspatial native ones.

.loadNet <- function(file, description = rev(unlist(strsplit(file, "/")))[1], details = 0) {
    if(endsWith(file, '.xdsl')){
        plist <- .read_xdsl(file)
    } else if (endsWith(file, '.dne')){
        plist <- .read_dne(file)
    } else {
        xxx <- .readHugin(file, details)
        xxx$nodeList <- lapply(xxx$nodeList, .fixLines) ## This line fixes Genie quirk when saving .net files
        yyy <- .transformHuginNet2internal(xxx)
        universe <- .asUniverse(yyy)
        plist <- lapply(yyy$potentialList, .hpot2cptable, universe)
    }
    return(gRain::grain(gRain::compileCPT(plist)) )
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
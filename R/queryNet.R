#' @name queryNet
#' @title Query the Bayesian network
#'
#' @description This function queries the Bayesian network and returns the probabilities for each 
#' state of the target node. Available input variables are set as evidence.\cr 
#' \code{queryNetParallel} works as \code{queryNet}, but makes use of multi cores/processors facilities 
#' for big network queries, by splitting data into chunks and processing them in parallel.
#' @aliases queryNetParallel
#' @rdname queryNet
#' @inheritParams loadNetwork
#' @param target character. The node of interest to be modelled and mapped.
#' @param evidence matrix or data.frame. Named columns are the known input variables; rows are the discrete states associated to them for each record (NA allowed).
#' @param ... Additional arguments to force one or more nodes to a state (i.e. fixing evidence), 
#' or rather, setting a node spatially equal everywhere. If the node is associated to input spatial data, the latter is ignored.
#' Node name is provided as argument and the associated fixed state as 
#' character; both node and state names must be typed accordingly to their names in the network.
#' @param inparallel logical or integer. Number of cores/processors to be used by \code{queryNetParallel}. 
#' Default is TRUE, so the maximum number available minus one is set.
#' @return A matrix of probabilities: columns are the target node states and rows are the probabilities associated to each record from argument \code{evidence} (e.g. spatial locations).
#' @examples
#' data(ConwyData)
#' list2env(ConwyData, environment())
#' 
#' network <- LandUseChange
#' 
#' q <- queryNet(network, 'FinalLULC', evidence)
#' head(q)
#' 
#' ## Fix a given node on a state (i.e. fixed evidence) by providing an additional argument
#' q <- queryNet(network, 'FinalLULC', evidence, Stakeholders = 'farmers')
#' head(q)
#' 
#' ## Fix two nodes, including one of the spatial inputs that gets overriden by fixed state
#' q <- queryNet(network, 'FinalLULC', evidence, Stakeholders = 'farmers', CurrentLULC = 'forest')
#' head(q)
#' ## For a programmatic approach, the arguments could be passed as named list:
#' # lst <- list(Stakeholders = 'farmers', CurrentLULC = 'forest')
#' # queryNet(network, 'FinalLULC', evidence, lst)
#' 
#' ## Use parallel processing
#' q <- queryNetParallel(network, 'FinalLULC', evidence, inparallel=2)
#' head(q)
#' 
#' @export
queryNet <- function(network, target, evidence, ...){
    .checkNames(network, target)
    nodesName <- network$universe$nodes
    if(is.data.frame(evidence)){
        evidence <- as.matrix(evidence)
    }
    evidence <- .freezeEvidence(evidence, network, ...)
    nms <- colnames(evidence)
    if(any(nms == target)){
        evidence <- evidence[, nms != target]
    }
    fixed <- nms[duplicated(nms)]
    if(length(fixed) > 0){
        for(i in fixed){
            fix <- max(which(nms == i))
            evidence[, nms == i] <- evidence[, fix]
            evidence <- evidence[ ,-fix]
        }
    }
    .checkPriors(network, evidence)
    inputNodes <- colnames(evidence) ## Harmonize with .freezeEvidence above
    if(any(inputNodes %in% nodesName == FALSE)){
        wrong = paste(inputNodes[!inputNodes %in% nodesName], collapse=', ')
        stop(paste('One or more nodes not found in the network, please check names:', wrong))
    }
    # Create single codes to identify all existing combinations of variables state
    # Codes are preferred as character type instead of numeric, although performance may be slightly affected
    singleCodes <- apply(evidence, 1, function(x) {paste(x, collapse="_")})
    uniCodes <- unique(singleCodes)
    # Query the network only once for each identified combinations, then append results to all corresponding cases
    evidenceSingle <- as.matrix(evidence[match(uniCodes, singleCodes), ])
    probs <- apply(evidenceSingle, 1, function(x){
        if(all(is.na(x))){
            out <- as.numeric(gRain::querygrain(network, target)[[1]] )
        } else {
            out <- as.numeric(gRain::querygrain(gRain::setEvidence(network, inputNodes, x), target)[[1]] )
        }
        if(any(is.nan(out) | is.infinite(out))){ # Extra check to cope with 
            stop('Impossible values have been set in the input network, ',
                 'e.g. zero as prior probability for an existing class.')
        }
        return(out)
    })
    probs <- t(probs)[match(singleCodes, uniCodes), ]
    colnames(probs) <- network$universe$levels[[target]]
    return(probs)
}

#' @rdname queryNet
#' @export
queryNetParallel <- function(network, target, evidence, inparallel=TRUE, ...){
    .checkNames(network, target)
    inparallel <- .inParallel(inparallel)
    if(!exists('tokenToHaltChildrenFromParallelProc', envir=parent.frame())){
        clst <- parallel::makeCluster(inparallel)
        doParallel::registerDoParallel(clst)
    }
    network <- loadNetwork(network=network)
    evidence <- cbind(evidence, ...)
    .checkPriors(network, evidence)
    splittedData <- split(as.data.frame(evidence), (seq(nrow(evidence))-1) %/% (nrow(evidence)/inparallel) )
    splittedData <- lapply(seq_along(splittedData), function(x){as.matrix(splittedData[[x]], ncol=ncol(evidence))})	
    i <- NULL # To remove NOTE from R package release check 
    #tab <- foreach::foreach(i = seq_along(splittedData), .combine=rbind, .packages=c("gRain", "bnspatial")) %dopar% {
    o <- foreach::foreach(i = seq_along(splittedData), .combine=rbind, .packages=c("gRain", "bnspatial"))
    tab <- foreach::"%dopar%"(o, {
        queryNet(network=network, target=target, evidence=splittedData[[i]])
    })
    if(!exists('tokenToHaltChildrenFromParallelProc', envir=parent.frame())){
        parallel::stopCluster(clst); gc()
    }
    return(tab)
}

## Append to evidence and return
.freezeEvidence <- function(tab, network, ...){ 
    ## Check if add. arg. is one of the provided nodes and substitute
    added <- list(...)
    if( length(added) == 1 ){
        added <- unlist(added, recursive=FALSE)
    }
    args <- names(added)
    .checkNames(network, args)
    tabNames <- colnames(tab)
    toFreeze <- added[which(args %in% tabNames)]
    for(nm in names(toFreeze)){
        tab[, nm] <- toFreeze[[nm]]
    }
    ## Check additional args: var name in node names
    nodes <- network$universe$nodes
    nodes <- nodes[!nodes %in% tabNames]
    toAdd <- added[args %in% nodes]
    if(length(toAdd) > 0){
        for(i in seq_along(toAdd)){
            nm <- names(toAdd[i])
            d <- toAdd[[i]]
            .checkStates(d, network$universe$levels[[nm]], nm)
            tab <- cbind(tab, d)
        }
        colnames(tab) <- c(tabNames, names(toAdd))
    }
    return(tab)	
}

## Check whether the input network contains impossible or potentially wrong values given the data
.checkPriors <- function(network, evidence){
    for(nm in colnames(evidence)){
        vals <- network$cptlist[[nm]]
        vvals <- as.vector(vals)
        if(any(vals < 0 | vals > 1) | sum(vvals) > length(vvals)){
            stop('Impossible probability values have been set in the input network for node: ', nm)
        }
        # if(any(vals == 0)){
        #     wrong <- names(vals)[vals == 0]
        #     if(any(evidence[ ,nm] == wrong)){
        #         stop('Cannot have zero prior probability for an existing class in the spatial data, nor for a fixed evidence state.\n  Check state "', wrong, '" of node "', nm, '"')
        #     }
        # }
    }
}


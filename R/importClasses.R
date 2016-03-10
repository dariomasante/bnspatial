# Author: Dario Masante , dmasan at ceh.ac.uk
# Date : February 2016
# Version 0
# Licence GPL v3

importClasses <- function(classFile){
	nodes <- vector()
	states <- list()
	classBoundaries <- list()
	classTxt <- scan(classFile, character(0), sep = "\n")
	for(i in seq(1, length(classTxt), by=3)){
		node <- classTxt[i]
		nodes <- c(nodes, node)
		state <- gsub(" ", "", unlist(strsplit(classTxt[i+1], ", ")))
		states[[node]] <- state
		class <- as.numeric(gsub(" ", "", unlist(strsplit(classTxt[i+2], ", "))))
		classBoundaries[[node]] <- class
	}
	setClasses(nodes=nodes, states=states, classBoundaries=classBoundaries)
}


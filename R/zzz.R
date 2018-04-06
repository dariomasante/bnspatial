.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        if (length(find.package('RBGL', quiet=TRUE)) == 0){
            packageStartupMessage('NOTE: to use "bnspatial" you need first to install "RGBL" package from Bioconductor.\n',
                                  'To install it, run the following line: \n',
                                  'source("http://bioconductor.org/biocLite.R"); biocLite("RBGL") \n',
                                  '\nbnspatial ', as.character(utils::packageVersion("bnspatial")),
                                  ' - For help type ?bnspatial or go to: cran.r-project.org/package=bnspatial \n',
                                  'License: GPL v3. Centre for Ecology and Hydrology and NERC (UK)\n', domain=NA, appendLF=TRUE)
        } else {
            packageStartupMessage('bnspatial ', as.character(utils::packageVersion("bnspatial")),
                                  ' - For help type ?bnspatial or go to: cran.r-project.org/package=bnspatial \n',
                                  'License: GPL v3. Centre for Ecology and Hydrology and NERC (UK)\n', domain=NA, appendLF=TRUE)
        }

    }
}
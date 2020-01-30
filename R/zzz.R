.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        msg <- paste(' - For help type ?bnspatial or go to: cran.r-project.org/package=bnspatial \n',
                     'License: GPL v3. Centre for Ecology and Hydrology.')
        if (length(find.package('RBGL', quiet=TRUE)) == 0){
            stop('To use "bnspatial" you need first to install "RGBL" package from Bioconductor.\n\n',
                 'To install on R version 3.6+: \n',
                 '    install.packages("BiocManager"); BiocManager::install("RBGL")  \n',
                 'For R version 3.5 only: \n',
                 '    install.packages("BiocManager"); BiocManager::install("RBGL", version = "3.8")  \n',
                 'For previous R versions (not recommended): \n',
                 '    source("http://bioconductor.org/biocLite.R"); biocLite("RBGL")\n')
        } else {
            packageStartupMessage('bnspatial ', as.character(utils::packageVersion("bnspatial")),
                                  msg, domain=NA, appendLF=TRUE)
        }

    }
}
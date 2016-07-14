.onAttach <- function(libname, pkgname) {
    if (interactive()) {
        packageStartupMessage('bnspatial ', as.character(utils::packageVersion("bnspatial")),' - For help type ?bnspatial\n',
                              'License: GPL-3. Centre for Ecology and Hydrology and NERC (UK)\n',
                              'Developed under European Union FP7 project "ROBIN" (http://robinproject.info)', domain=NA, appendLF=TRUE)
    }
}
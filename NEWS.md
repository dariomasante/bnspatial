bnspatial v1.1.1 (Release date: ... ... ...)
-----------------------------------------------
CHANGES:

* Entropy calculation fixed to vase 2 log and to adjust for certain outcome (prob. = 1) and zeros
* Minor changes in the internals to accomodate for changes in `gRain` and `gRbase` packages (mainly function `loadNetwork`)
* Smaller and simpler example vectorial data 
*


bnspatial v1.1 (Release date: 2019-08-02)
-----------------------------------------------
CHANGES:

* Vectorial spatial data now allowed, using package "sf" (all functions changed accordingly)
* Removed some redundancy in internal code, plus some cosmetics and minor edits in flow control inside hidden internal functions
* Added a shapefile to the example dataset, and changed some features in example rasters
* Performance improvements
* Some function arguments deprecated, but still back-compatible


bnspatial v1.0.5 (Release date: 2019-03-21)
-----------------------------------------------
CHANGES:

* Bug fix on mapTarget output probabilities under specific circumstances
* Add system of reference and remove names for all spatial outputs


bnspatial v1.0.4 (Release date: 2019-03-07)
-----------------------------------------------
CHANGES:

* Updated to R 3.5 with Bioconductor 3.8 installation notes


bnspatial v1.0.3 (Release date: 2017-11-16)
-----------------------------------------------
CHANGES:

* Added exception handling and error messages for impossible input values
* Changed an internal variable name in function queryNet to avoid mantainer confusion 


bnspatial v1.0.2 (Release date: 2017-07-21)
-----------------------------------------------
CHANGES:

* `aoi` function now accepts character vectors (i.e. input files paths)
* Example data compacted into a list for neater index of package content
* Minor typos corrected in documentation
* New mantainer's email address in the DESCRIPTION file


bnspatial v1.0.1 (Release date: 2016-12-14)
-----------------------------------------------
CHANGES:

* New mantainer's email address in the DESCRIPTION file
* Minor fix to html documentation format
* Slight speed improvements with better use of internal functions


bnspatial v1.0 (Release date: 2016-07-27)
-----------------------------------------------
CHANGES:

* Function `mapTarget` (and wrapper `bnspatial`) improved to write large spatial data without oveloading memory
* Improved robustness in handling input arguments and error/warning messages for most functions
* Swapped an internal function to compile the network inside `loadNetwork`
* Changed objects name within example dataset to show tidily in documentation and minor changes to examples
* Added NEWS.md file
* Added copyright notes in COPYRIGHT file
* Fixed author list to CRAN standards
* Improved and extended documentation (and code comments)
* Added startup message with copyright notice and acknowledgments



bnspatial v0.9 (Release date: 2016-06-02)
-----------------------------------------------

NEW FEATURES:

* First release on CRAN

##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   13.07.2018
#
# purpose:
#   - install R packages required for the CI jobs
#   - install the repository version of hydflood3
#
##################################################

# configure output
verbose <- TRUE
quiet <- !verbose

# update.packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# install dependencies
packages <- c("hyd1d", "rgdal", "sp", "raster", "rgeos", "Rdpack", "knitr",  
              "rmarkdown", "devtools", "pkgdown", "roxygen2", "testthat",
              "plot3D", "plotrix", "shiny", "shinyjs", "shinycssloaders", 
              "leaflet", "leaflet.extras", "leaflet.esri", "htmltools",
              "usethis")

for (a_package in packages) {
    if (! (a_package %in% installed.packages()[, "Package"])) {
        install.packages(a_package, dependencies = TRUE, quiet = quiet)
    }
}

# install the local package
require(devtools)
devtools::install(".", reload = FALSE, quick = TRUE, quiet = quiet, 
                  dependencies = TRUE)

# exit
q("no")

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

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")
dir.create(lib, verbose, TRUE)

# install dependencies
packages <- c("hyd1d", "rgdal", "sp", "raster", "rgeos", "Rdpack", "knitr",  
              "rmarkdown", "devtools", "pkgdown", "roxygen2", "testthat",
              "plot3D", "plotrix", "shiny", "shinyjs", "shinycssloaders", 
              "leaflet", "leaflet.extras", "leaflet.esri", "htmltools",
              "usethis")

for (a_package in packages) {
    if (! (a_package %in% installed.packages(lib.loc = lib)[, "Package"])) {
        install.packages(a_package, lib = lib, 
                         repos = "https://ftp.gwdg.de/pub/misc/cran/", 
                         dependencies = TRUE, quiet = quiet)
    }
}

# update.packages
update.packages(lib.loc = lib, ask = FALSE)

# install the local package
require(devtools, lib.loc = lib)
devtools::install(".", reload = FALSE, quick = TRUE, 
                  args = paste0("--library=", lib), quiet = quiet, 
                  dependencies = FALSE)

# exit
q("no")


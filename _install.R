##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   05.02.2019
#
# purpose:
#   - install R packages required for the CI jobs
#   - install the repository version of hydflood
#
##################################################

# update.packages
update.packages(lib.loc = .libPaths()[1], ask = FALSE, checkBuilt = TRUE)

# install all possible dependencies
packages <- c("hyd1d", "sf", "tidyverse", "raster", "terra", "Rdpack", 
              "rmarkdown", "devtools", "pkgdown", "roxygen2", "testthat",
              "plot3D", "plotrix", "shiny", "shinyjs", "shinycssloaders",
              "leaflet", "leaflet.extras", "leaflet.esri", "htmltools",
              "usethis", "lattice", "pangaear", "rgrass", "knitr",
              "shiny.i18n", "prettymapr", "ggplot2", "maps", "rosm",
              "sfheaders", "rnaturalearth", "rnaturalearthdata", "ggspatial",
              "ggmap")

for (a_package in packages) {
    if (! (a_package %in% installed.packages()[, "Package"])) {
        install.packages(a_package, dependencies = TRUE)
    }
}

# install the local package
library(devtools)
devtools::install(".", quick = TRUE)  #, dependencies = TRUE)

# rnaturalearthhires
if (!require("rnaturalearthhires")) {
    install.packages("rnaturalearthhires",
                     repos = "https://ropensci.r-universe.dev",
                     type = "source")
}

# exit
q("no")

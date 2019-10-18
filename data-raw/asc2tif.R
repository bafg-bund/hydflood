##################################################
# dem2tif.R
#
# author: arnd.weber@bafg.de
# date:   27.09.2019
#
# purpose: 
#   - convert dem raster datasets from asc to tif
#
##################################################
types <- c("DEM", "CSA")
rivers <- c("Elbe", "Rhein", "Weser")

for (a_type in types) {
    if (a_type == "DEM") {
        ot <- "Float64"
    } else {
        ot <- "Int32"
    }
    for (a_river in rivers) {
        in_dir <- paste0("/home/WeberA/flut3_", a_river, "/data/ascii")
        ou_dir <- paste0("/home/WeberA/flut3_", a_river, "/data/tiff")
        files <- list.files(in_dir, pattern = glob2rx(paste0("*_", a_type,
                                                             ".asc")))
        if (a_river == "Elbe") {
            crs <- "EPSG:25833"
        } else {
            crs <- "EPSG:25832"
        }
        for (a_file in files) {
            in_file <- paste0(in_dir, "/", a_file)
            ou_file <- paste0(ou_dir, "/", gsub(".asc", ".tif", a_file,
                                                fixed = TRUE))
            print(in_file)
            if (!file.exists(ou_file)) {
                system(paste0("gdal_translate -q -ot ", ot, " -co \"COMPRESS=L",
                              "ZW\" -of \"GTiff\" -a_srs ", crs, " ", in_file,
                              " ", ou_file))
            }
        }
    }
}


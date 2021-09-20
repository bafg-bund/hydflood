#####
# get TASK_ID
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#####
# output directory
o <- "/scratch/webera/flood3"
dir.create(o, FALSE, FALSE)

#####
# load R packages
library(raster)
rasterOptions(tmpdir = "/scratch/webera/raster//")
rasterOptions(chunksize = 1e8)
rasterOptions(maxmemory = 5e9)
rasterOptions()
options("rgdal_show_exportToProj4_warnings" = "none")
library(rgdal)
library(hyd1d, lib.loc = paste0("~/R/", getRversion()))
library(hydflood, lib.loc = paste0("~/R/", getRversion()))

# subset spdf.tiles_elbe
spdf.tile <- spdf.tiles_elbe[i, ]
hfc <- hydflood_cache$cache_path_get()
n <- spdf.tile$name
# if (!n %in% c(#"e036_SANDAU2", "e038_WITTENBERGE", "e039_MUEGGENDORF",
#              #"e040_SCHNACKENBURG", "e041_LENZEN", "e042_GORLEBEN",
#              "e043_DOEMITZ" #, "e044_HITZACKER", "e045_NEU_DARCHAU",
#              #"e046_BLECKEDE")) {
#              )) {
#   stdout("not included in the computations")
#   q("no")
# }
dem <- paste0(hfc, "/", n, "_DEM.tif")
csa <- paste0(hfc, "/", n, "_CSA.tif")

#####
# loop over the available years to produce annual flood3 rasters
products <- list()

for (a_year in 1960:(as.numeric(strftime(Sys.Date(), "%Y")) - 1)) {
  print(a_year)
  
  # output file
  f <- paste0(o, "/", n, "_flood3_", a_year, ".tif")
  
  # collect filenames for aggregation to mean and sd
  products <- append(products, f)
  names(products)[length(products)] <- paste0("flood3_", a_year)
  
  # compute ?!?
  if (file.exists(f)) {
    print(paste0(f, " exists already"))
  } else {
    print(paste0(f, " will be computed"))
    x <- hydflood::hydRasterStack(filename_dem = dem, filename_csa = csa)
    s <- seq.Date(from = as.Date(paste0(a_year, "-01-01")),
                  to = as.Date(paste0(a_year, "-12-31")), by = "day")
    print(paste0(length(s), " days in ", a_year))
    hydflood::flood3(x, s, filename = f, format = "GTiff",
                     options = c("COMPRESS=LZW", "TFW=NO"))
  }
  print("")
}

#####
# aggregate selected years by mean() and sd()
# annual prolongation of temporal series from 1990 until last year
f_mean <- paste0(o, "/", n, "_flood3_mean_1990_",
                 as.numeric(strftime(Sys.Date(), "%Y")) - 1, ".tif")

if (file.exists(f_mean)) {
  print(paste0(f_mean, " exists already"))
} else {
  print(paste0(f_mean, " will be computed"))
  calc(stack(products), fun = mean, na.rm = TRUE, filename = f_mean,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

f_sd <- paste0(o, "/", n, "_flood3_sd_1990_",
               as.numeric(strftime(Sys.Date(), "%Y")) - 1, ".tif")

if (file.exists(f_sd)) {
  print(paste0(f_sd, " exists already"))
} else {
  print(paste0(f_sd, " will be computed"))
  calc(stack(products), fun = sd, na.rm = TRUE, filename = f_sd,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

# reference period 1960 - 1989
f_mean <- paste0(o, "/", n, "_flood3_mean_1960_1989.tif")

if (file.exists(f_mean)) {
  print(paste0(f_mean, " exists already"))
} else {
  print(paste0(f_mean, " will be computed"))
  calc(stack(products[paste0("flood3_", 1960:1989)]),
       fun = mean, na.rm = TRUE, filename = f_mean,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

f_sd <- paste0(o, "/", n, "_flood3_sd_1960_1989.tif")

if (file.exists(f_sd)) {
  print(paste0(f_sd, " exists already"))
} else {
  print(paste0(f_sd, " will be computed"))
  calc(stack(products[paste0("flood3_", 1960:1989)]),
       fun = sd, na.rm = TRUE, filename = f_sd,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

# reference period 1990 - 2019
f_mean <- paste0(o, "/", n, "_flood3_mean_1990_2019.tif")

if (file.exists(f_mean)) {
  print(paste0(f_mean, " exists already"))
} else {
  print(paste0(f_mean, " will be computed"))
  calc(stack(products[paste0("flood3_", 1990:2019)]),
       fun = mean, na.rm = TRUE, filename = f_mean,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

f_sd <- paste0(o, "/", n, "_flood3_sd_1990_2019.tif")

if (file.exists(f_sd)) {
  print(paste0(f_sd, " exists already"))
} else {
  print(paste0(f_sd, " will be computed"))
  calc(stack(products[paste0("flood3_", 1990:2019)]),
       fun = sd, na.rm = TRUE, filename = f_sd,
       format = "GTiff", options = c("COMPRESS=LZW", "TFW=NO"))
}

#####
# quit
q("no")

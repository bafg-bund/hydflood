library(hydflood)
library(microbenchmark)

# import the raster data and create a raster stack
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem.tif",
                   filename_csa = "data-raw/raster.csa.tif")

# create a temporal sequence
seq <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")

# compute a flood duration
mbm <- microbenchmark(
    "flood3" = {
        fd <- flood3(x = x, seq = seq)
    },
    times = 100
)

mbm
mbm <- as.data.frame(mbm)
mean(mbm$time/1000000000)
sd(mbm$time/1000000000)

save(mbm, file = "vignettes/microbenchmarking.Rdata")


################################################################################
# movie_mp4.R
#
# author: arnd.weber@bafg.de
# date:   26.07.2018
#
# purpose:
#   - plot a sequence of 3D plots with water level data near Dessau using 
#     'movie_jpg.R'
#   - combine this sequence to an mpeg4-video
#
################################################################################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
    stop("One argument must be supplied (year)!\n", 
         call. = FALSE)
} else {
    year <- args[1]
}

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# output paths
out_dir <- paste0("vignettes/movie/lenzen/", year,"/")
dir.create(out_dir, verbose, TRUE)

# convert the jpg-files to an mp4 video
system(paste0("ffmpeg -y -framerate 1 -i ", out_dir, "flood3_%03d.jpg -c:v ",
              "libx264 -r 30 -pix_fmt yuv420p ", out_dir, "flood3-1fps.mp4"))

system(paste0("ffmpeg -y -framerate 2 -i ", out_dir, "flood3_%03d.jpg -c:v ",
              "libx264 -r 30 -pix_fmt yuv420p ", out_dir, "flood3-2fps.mp4"))

# quit R
q("no")


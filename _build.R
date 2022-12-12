##################################################
# _build.R
#
# author: arnd.weber@bafg.de
# date:   05.02.2019
#
# purpose: 
#   - build the repository version of hydflood
#
##################################################
write("#####", stdout())
write(" set en_US locale", stdout())
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_PAPER", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MEASUREMENT", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.UTF-8")

#####
# assemble variables, create output directories and load packages
write("#####", stdout())
write(" R variables", stdout())

# obtain R version
R_version <- getRversion()

# assemble and create R version specific output paths
build <- paste0("built/", R_version)
dir.create(build, FALSE, TRUE)
public <- "docs/"
dir.create(public, FALSE, TRUE)

#####
# load the packages
write("#####", stdout())
write(" load packages", stdout())
require(devtools)
require(usethis)
require(sf)
require(terra)
require(knitr)
require(rmarkdown)
require(pkgdown)
require(hyd1d)
require(hoardr)

# set standard projections
wgs84 <- st_crs(4326)
utm32n <- st_crs(25832)
utm33n <- st_crs(25833)

#####
# session info
write("#####", stdout())
write(" sessionInfo", stdout())
sessionInfo()

#####
# package the data, if necessary ...
# - reversed order, since some datasets are passed between individual
#   sourced scripts
write("#####", stdout())
write(" data-raw", stdout())
for (a_file in rev(list.files("data-raw", pattern = "data_*",
                              full.names = TRUE))) {
    source(a_file)
}
rm(a_file)

#####
# minimal devtools workflow
write("#####", stdout())
write(" load_all", stdout())
devtools::load_all(".")

#####
# build documentation
write("#####", stdout())
write(" document", stdout())
devtools::document(".")

#####
# build vignettes
write("#####", stdout())
write(" build vignettes", stdout())
devtools::build_vignettes(".")
tools::compactPDF(paths = "doc", gs_quality = "ebook")

#####
# check the package source
write("#####", stdout())
write(" check", stdout())
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
devtools::check(".", document = TRUE, manual = TRUE, error_on = "never",
                build_args = c('--compact-vignettes=both'))

#####
# build the source package
write("#####", stdout())
write(" build", stdout())
devtools::build(".", path = build, vignettes = TRUE, manual = TRUE,
                args = c("--compact-vignettes=both"))

#####
# install hyd1d from source
write("#####", stdout())
write(" install from source", stdout())

pkg_files <- list.files(path = build,
                        pattern = paste0("hydflood\\_[:0-9:]\\.[:0-9:]\\.[:0-9",
                                         ":]\\.tar\\.gz"))

for (a_file in pkg_files) {
    
    write(a_file, stdout())
    
    # seperate package name from version string
    package_name <- unlist(strsplit(a_file, "_"))[1]
    package_version <- gsub(".tar.gz", "", unlist(strsplit(a_file, "_"))[2])
    
    # check presently installed local packages
    pkgs <- as.data.frame(installed.packages())
    if (package_name %in% pkgs$Package) {
        if (compareVersion(as.character(packageVersion(package_name)),
                           package_version) < 1) {
            install.packages(paste(build, a_file, sep = "/"), 
                             dependencies = TRUE)
        }
    } else {
        install.packages(paste(build, a_file, sep = "/"), dependencies = TRUE)
    }
}

#####
# document
write("#####", stdout())
write(" document gitlab & website", stdout())

# render the README.md
if (!(file.exists("README.md"))) {
    rmarkdown::render("README.Rmd", output_format = "github_document",
                      output_file = "README.md", clean = TRUE)
    unlink("README.html", force = TRUE)
}

# render the package website
pkgdown::build_site(".", examples = TRUE, preview = FALSE, new_process = TRUE)

# insert the BfG logo into the header
files <- list.files(path = public, pattern = "*[.]html",
                    all.files = TRUE, full.names = FALSE, recursive = TRUE,
                    ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
for (a_file in files) {
    x <- readLines(paste0(public, a_file))
    if (grepl("/", a_file, fixed = TRUE)){
        write(a_file, stdout())
        y <- gsub('href="https://www.bafg.de">BfG</a>',
                  paste0('href="https://www.bafg.de"><img border="0" src="..',
                         '/bfg_logo.jpg" height="50px" width="114px"></a>'), x)
    } else {
        y <- gsub('href="https://www.bafg.de">BfG</a>',
                  paste0('href="https://www.bafg.de"><img border="0" src="bf',
                         'g_logo.jpg" height="50px" width="114px"></a>'), x)
    }
    # edit footer
    y <- gsub('Developed by Arnd Weber.',
              paste0('Developed by Arnd Weber. <a href="ht',
                     'tps://www.bafg.de/EN/Service/Imprint/imprint_node.html">',
                     'Imprint</a>.'),
              y)
    # remove the prefix "technical report" in references
    z <- gsub('Technical Report ', '', y)
    cat(z, file = paste0(public, a_file), sep="\n")
}

# clean up
rm(a_file, files, x, y, z)

# copy logo
if (!(file.exists(paste0(public, "bfg_logo.jpg")))) {
    file.copy("pkgdown/bfg_logo.jpg", public)
}

# copy README_files into public
dir.create(paste0(public, "README_files/figure-markdown_github"), FALSE, TRUE)
file.copy("README_files/figure-markdown_github/usage-1.png", 
          paste0(public, "README_files/figure-markdown_github"))

#####
# create docs/downloads directory and copy hydflood_*.tar.gz-files into it
downloads <- paste0(public, "downloads")
dir.create(downloads, FALSE, TRUE)
from <- list.files(path = build,
                   pattern = "hydflood\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]\\.tar\\.gz",
                   full.names = TRUE)
file.copy(from = from, to = downloads, overwrite = TRUE, copy.date = TRUE)
file.copy(from = "data-raw/sf.afe_csa.rda", to = downloads, overwrite = FALSE,
          copy.date = TRUE)
file.copy(from = "data-raw/sf.afr_csa.rda", to = downloads, overwrite = FALSE,
          copy.date = TRUE)

#####
# export the documentation as pdf
write("#####", stdout())
write(" export the documentation as pdf", stdout())

system(paste0("R CMD Rd2pdf . --output=", downloads, "/hydflood.pdf --no-prev",
              "iew --force --RdMacros=Rdpack --encoding=UTF-8 --outputEncoding",
              "=UTF-8"))

#####
# document
# user, nodename and version dependent sync to web roots and install directories
write("#####", stdout())
write(" web", stdout())

host <- Sys.info()["nodename"]
user <- Sys.info()["user"]
if (host == "pvil-r" & user == "WeberA" & R_version == "4.2.2") {
    # copy html output to ~/public_html
    system(paste0("cp -rp ", public, "* /home/", user, "/public_htm",
                  "l/hydflood/"))
    system("permissions_html")
    
    # copy shinyapps to ~/ShinyApps
    system(paste0("cp -rp shinyapps/flood3/* /home/", user, "/ShinyApps/",
                  "07-flood3"))
    system(paste0("cp -rp shinyapps/flood3daily/* /home/", user, "/ShinyApps/",
                  "10-flood3daily"))
    system(paste0("cp -rp shinyapps/flood3wms/* /home/", user,
                  "/ShinyApps/08-flood3wms"))
    system("permissions_shiny")
    
    # copy package source to r.bafg.de
    system(paste0("[ -d /home/", user, "/freigaben_r/_packages/package_sources",
                  " ] && cp -rp ", public, "downloads/hydflood_*.tar.gz /home/",
                  user, "/freigaben_r/_packages/package_sources"))
}

#q("no")


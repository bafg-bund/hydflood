##################################################
# _build.R
#
# author: arnd.weber@bafg.de
# date:   20.07.2018
#
# purpose: 
#   - build the repository version of hyd1d
#
##################################################

# configure output
verbose <- TRUE
quiet <- !verbose

#####
# assemble variables, create output directories and load packages
write("#####", stdout())
write(" R variables", stdout())

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# output paths
build <- paste0("build/", R_version)
dir.create(build, verbose, TRUE)
public <- paste0("public/", R_version)
dir.create(public, verbose, TRUE)
downloads <- paste0("public/", R_version, "/downloads")
dir.create(downloads, verbose, TRUE)

#####
# load the packages
write("#####", stdout())
write(" load packages", stdout())
require(devtools, lib.loc = lib)
require(sp, lib.loc = lib)
require(raster, lib.loc = lib)
require(rgdal, lib.loc = lib)
require(rgeos, lib.loc = lib)
require(knitr, lib.loc = lib)
require(rmarkdown, lib.loc = lib)
require(pkgdown, lib.loc = lib)
require(hyd1d, lib.loc = lib)

#####
# assemble variables and create output directories
write("#####", stdout())
write(" set en_US locale", stdout())
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_PAPER", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MEASUREMENT", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.UTF-8")

#####
# assemble variables and create output directories
write("#####", stdout())
write(" sessionInfo", stdout())
sessionInfo()

#####
# package the data, if necessary ...
# - reversed order, since some datasets are passed between individual
#   sourced scripts
write("#####", stdout())
write(" data-raw", stdout())
for (a_file in rev(list.files("data-raw", pattern = "data_spdf.*",
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

#####
# check the package source
write("#####", stdout())
write(" check", stdout())
devtools::check(".", document = FALSE, manual = FALSE,
                build_args = "--no-build-vignettes")

#####
# build the source package
write("#####", stdout())
write(" build", stdout())
devtools::build(".", path = build, vignettes = FALSE, manual = FALSE)

#####
# create public/downloads directory and copy hyd1d_*.tar.gz-files into it
from <- list.files(path = build,
                   pattern = "hydflood3\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]\\.tar\\.gz",
                   full.names = TRUE)
file.copy(from = from, to = downloads, overwrite = TRUE, copy.date = TRUE)

#####
# install hyd1d from source
write("#####", stdout())
write(" install from source", stdout())

pkg_files <- list.files(path = build,
                        pattern = paste0("hydflood3\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]",
                                         "\\.tar\\.gz"))

for (a_file in pkg_files) {
    
    write(a_file, stdout())
    
    # seperate package name from version string
    package_name <- unlist(strsplit(a_file, "_"))[1]
    package_version <- gsub(".tar.gz", "", unlist(strsplit(a_file, "_"))[2])
    
    # check presently installed local packages
    pkgs <- as.data.frame(installed.packages(lib.loc = lib))
    if (package_name %in% pkgs$Package) {
        if (compareVersion(as.character(packageVersion(package_name,
                                                       lib.loc = lib)),
                           package_version) < 1) {
            install.packages(paste(build, a_file, sep = "/"),
                             lib = lib, dependencies = TRUE, quiet = quiet)
        }
    } else {
        install.packages(paste(build, a_file, sep = "/"),
                         lib = lib, dependencies = TRUE, quiet = quiet)
    }
}

#####
# export the documentation as pdf
write("#####", stdout())
write(" export the documentation as pdf", stdout())

system(paste0("R CMD Rd2pdf . --output=", downloads, "/hydflood3.pdf --no-preview ",
              "--force --RdMacros=Rdpack --encoding=UTF-8 --outputEncoding=UTF",
              "-8"), ignore.stdout = quiet, ignore.stderr = quiet)

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
#pkgdown::clean_site(".")
pkgdown::build_site(".", examples = TRUE, preview = FALSE, document = FALSE, 
                    override = list(destination = public), new_process = FALSE)

if (file.exists("_pkgdown.yml")) {
    file.remove("_pkgdown.yml")
}
if (dir.exists("docs")) {
    unlink("docs", TRUE, TRUE)
}

# insert the BfG logo into the header
files <- list.files(path = public, pattern = "*[.]html",
                    all.files = TRUE, full.names = FALSE, recursive = TRUE,
                    ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
for (a_file in files){
    x <- readLines(paste0(public, "/", a_file))
    if (grepl("/", a_file, fixed = TRUE)){
        if (verbose) {
            write(a_file, stdout())
        }
        y <- gsub('<a href="http://www.bafg.de">BfG</a>',
                  paste0('<a href="http://www.bafg.de"><img border="0" src="..',
                         '/bfg_logo.jpg" height="50px" width="114px"></a>'), x)
    } else {
        y <- gsub('<a href="http://www.bafg.de">BfG</a>',
                  paste0('<a href="http://www.bafg.de"><img border="0" src="bf',
                         'g_logo.jpg" height="50px" width="114px"></a>'), x)
    }
    # remove the prefix "technical report" in references
    z <- gsub('Technical Report ', '', y)
    cat(z, file = paste0(public, "/", a_file), sep="\n")
}

# clean up
rm(a_file, files, x, y)

# copy logo
if (!(file.exists(paste0(public, "/bfg_logo.jpg")))){
    file.copy("pkgdown/bfg_logo.jpg", public)
}

#####
# document
# user, nodename and version dependent sync to web roots and install directories
write("#####", stdout())
write(" web", stdout())

host <- Sys.info()["nodename"]
user <- Sys.info()["user"]
if (host == "hpc-service" & user == "WeberA" & R_version == "3.5.0") {
    system(paste0("cp -rp public/", R_version, "/* /home/", user, 
                  "/public_html/hydflood3/"))
    system(paste0("find /home/", user, "/public_html/hydflood3/ -type f -print",
                  "0 | xargs -0 chmod 0644"))
    system(paste0("find /home/", user, "/public_html/hydflood3/ -type d -print",
                  "0 | xargs -0 chmod 0755"))
    system(paste0("chcon -R -t httpd_user_content_t /home/", user,
                  "/public_html/"))
    system(paste0("[ -d /home/", user, "/freigaben/AG/R/server/server_admin/pa",
                  "ckage_sources ] && cp -rp public/", R_version, "/downloads/",
                  "hydflood3_*.tar.gz /home/", user, "/freigaben/AG/R/server/s",
                  "erver_admin/package_sources"))
}

q("no")


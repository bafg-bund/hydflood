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
library(devtools)
library(usethis)
library(sf)
library(terra)
library(knitr)
library(rmarkdown)
library(pkgdown)
library(hyd1d)
library(bslib)
library(xml2)

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
# install hydflood from source
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
write(" document git & website", stdout())

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

for (a_file in files){
    
    write(a_file, stdout())
    
    # skip presentations
    if (startsWith(a_file, "articles/presentation")) {next}
    
    # prefix
    if (grepl("/", a_file, fixed = TRUE)){
        pref <- "../"
    } else {
        pref <- ""
    }
    
    # place logo in navbar
    x <- readLines(paste0(public, a_file))
    y <- gsub('href="https://www.bafg.de">BfG</a>',
              paste0('href="https://www.bafg.de"><img class="bfglogo" border="',
                     '0" src="', pref, 'bfg_logo.png" height="50px" width="98',
                     'px"></a>'), x)
    
    # edit footer
    y <- gsub('Developed by Arnd Weber.',
              paste0('Developed by Arnd Weber. <a href="https://www.bafg.de/EN',
                     '/Service/Imprint/imprint_node.html">Imprint</a>.'), y)
    
    # remove the prefix "technical report" in references
    z <- gsub('Technical Report ', '', y)
    
    # export html
    cat(z, file = paste0(public, a_file), sep="\n")
    
    # replace external js and css sources
    page <- read_html(paste0(public, a_file), "utf-8")
    
    # scripts
    scripts <- xml_find_all(page, ".//script")
    scripts_src <- xml_attr(scripts, "src")
    scripts_replace <- character()
    for (script in scripts_src) {
        if (is.na(script)) {
            next
        } else if (startsWith(script, "https://hydflood.bafg.de/") |
                   startsWith(script, "../") |
                   startsWith(script, "deps/") |
                   startsWith(script, "pkgdown.js")) {
            script <- gsub("https://hydflood.bafg.de/", "" , script,
                           fixed = TRUE)
            script <- gsub("../", "" , script, fixed = TRUE)
            scripts_replace <- append(scripts_replace, paste0(pref, script))
            next
        } else if (startsWith(script, "https://cdn.jsdelivr.net/gh/afeld/")) {
            scriptt <- gsub("https://cdn.jsdelivr.net/gh/afeld/", "", script,
                            fixed = TRUE)
        } else if (startsWith(script,
                              "https://cdnjs.cloudflare.com/ajax/libs/")) {
            scriptt <- gsub("https://cdnjs.cloudflare.com/ajax/libs/", "",
                            script, fixed = TRUE)
        } else {
            stop(paste0(script, " is not known yet"))
        }
        
        destfile <- paste0(public, "deps/", scriptt)
        if (!file.exists(destfile)) {
            # create storage directory
            pos <- unlist(gregexpr("/", scriptt, fixed = TRUE))
            scriptth <- substr(scriptt, 1, pos[length(pos)])
            dir.create(paste0(public, "deps/", scriptth), FALSE, TRUE)
            
            # download file to that directory
            download.file(script, destfile, method = "curl", quiet = TRUE)
        }
        
        # replace path in html file
        script_replace <- paste0("deps/", scriptt)
        scripts_replace <- append(scripts_replace, paste0(pref, script_replace))
    }
    xml_attr(scripts, "src")[xml_has_attr(scripts, "src")] <- scripts_replace
    xml_attr(scripts, "integrity") <- NULL
    xml_attr(scripts, "crossorigin") <- NULL
    
    # links
    links <- xml_find_all(page, ".//link")
    links_href <- xml_attr(links, "href")
    links_replace <- character()
    for (link in links_href) {
        if (is.na(link)) {
            next
        } else if (startsWith(link, "https://hydflood.bafg.de/") |
                   startsWith(link, "../") |
                   startsWith(link, "deps/") |
                   startsWith(link, "extra.css") |
                   startsWith(link, "favicon-") |
                   startsWith(link, "apple-touch-")) {
            link <- gsub("https://hydflood.bafg.de/", "" , link, fixed = TRUE)
            link <- gsub("../", "" , link, fixed = TRUE)
            links_replace <- append(links_replace, paste0(pref, link))
            next
        } else if (startsWith(link,
                              "https://cdnjs.cloudflare.com/ajax/libs/")) {
            linkt <- gsub("https://cdnjs.cloudflare.com/ajax/libs/", "",
                          link, fixed = TRUE)
        } else {
            stop(paste0(link, " is not known yet"))
        }
        
        destfile <- paste0(public, "deps/", linkt)
        if (!file.exists(destfile)) {
            # create storage directory
            pos <- unlist(gregexpr("/", linkt, fixed = TRUE))
            linkth <- substr(linkt, 1, pos[length(pos)])
            dir.create(paste0(public, "deps/", linkth), FALSE, TRUE)
            
            # download file to that directory
            download.file(link, destfile, method = "curl", quiet = TRUE)
        }
        
        # replace path in html file
        link_replace <- paste0("deps/", linkt)
        links_replace <- append(links_replace, paste0(pref, link_replace))
    }
    xml_attr(links, "href")[xml_has_attr(links, "href")] <- links_replace
    xml_attr(links, "integrity") <- NULL
    xml_attr(links, "crossorigin") <- NULL
    
    # store changes
    write_html(page, paste0(public, a_file), encoding = "utf-8")
    
}

# download additional missing files
dir <- paste0(public, "deps/font-awesome/5.12.1/webfonts/")
dir.create(dir, FALSE, TRUE)
files <- c("fa-brands-400.woff2", "fa-brands-400.woff", "fa-brands-400.ttf",
           "fa-solid-900.woff2", "fa-solid-900.woff", "fa-solid-900.ttf")
for (a_file in files) {
    if (!file.exists(paste0(dir, a_file))) {
        u <- paste0("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.",
                    "1/webfonts/", a_file)
        destfile <- paste0(dir, a_file)
        download.file(u, destfile, method = "curl", quiet = TRUE)
    }
}

# clean up
rm(a_file, files, x, y, z, pref, page,
   scripts, script, scriptt, script_replace, scripts_replace, scripts_src,
   links, link, linkt, link_replace, links_replace, links_href, destfile)
if (exists("linkth")) {rm(linkth)}
if (exists("scriptth")) {rm(scriptth)}
if (exists("pos")) {rm(pos)}
if (exists("u")) {rm(u)}

# copy logo
if (!(file.exists(paste0(public, "bfg_logo.png")))){
    file.copy("pkgdown/bfg_logo.png", public)
}

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
if (host == "pvil-r" & user == "WeberA" & R_version == "4.3.2") {
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


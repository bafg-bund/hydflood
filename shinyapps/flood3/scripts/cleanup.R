################################################################################
# cleanup.R
#
# author: arnd.weber@bafg.de
# date:   12.09.2018
#
# purpose: 
#   - delete products older than 7 days
#
################################################################################
setwd("/home/WeberA/ShinyApps/07-flood3/")

write("", stdout())
write("#############################################################", stdout())
write(strftime(Sys.Date(), "%Y-%-m-%d"), stdout())
write("#############################################################", stdout())
# move the *.log files
write("log-files", stdout())
for (a_file in list.files("in_process", pattern = "*.log")) {
    id <- gsub(".log", "", a_file)
    dir.create(paste0("processed/", id), FALSE, TRUE)
    write(paste0("in_process/", id, ".log"), stdout())
    file.rename(from = paste0("in_process/", id, ".log"), 
                to = paste0("processed/", id, "/", id, ".log"))
}
write("", stdout())

# delete the expired zip files
write("data-files", stdout())
for (a_file in list.files("www/downloads", pattern = "*.zip")) {
    if (file.mtime(paste0("www/downloads/", a_file)) < 
        Sys.time() - 8 * 24 * 60 * 60) {
        # delete the zip file
        write(paste0("www/downloads/", a_file), stdout())
        unlink(paste0("www/downloads/", a_file), force = TRUE)
        
        # delete the *.tif, *.tif.xml and *.msg
        id <- gsub(".zip", "", a_file)
        for (b_file in list.files(paste0("processed/", id, "/"))) {
            if (endsWith(".RData", b_file)) {next}
            write(paste0("processed/", id, "/", b_file), stdout())
            unlink(paste0("processed/", id, "/", b_file), force = TRUE)
        }
    }
}

# quit R
q("no")

# store df.pnv as external dataset
if (!(file.exists("data/df.pnv.rda"))) {
    
    # import
    df.pnv <- read.table("data-raw/df.pnv.csv", header = TRUE, sep = ";")
    
    # export
    usethis::use_data(df.pnv, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(df.pnv)
    
} else {
    write("data/df.pnv.rda exists already", stderr())
}

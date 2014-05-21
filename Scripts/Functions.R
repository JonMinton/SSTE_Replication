

# A not-very-elegant function taken from download_source
# TO DO: add proper version control by using the sha1 argument correctly
download_file_url <- function (
    url, 
    outfile,
    ..., sha1 = NULL) 
{
    require(RCurl)
    require(devtools)
    require(repmis)
    require(httr)
    require(digest)
    
    stopifnot(is.character(url), length(url) == 1)
    filetag <- file(outfile, "wb")
    request <- GET(url)
    stop_for_status(request)
    writeBin(content(request, type = "raw"), filetag)
    close(filetag)
}


Unpack_HMD <- function(
    zipfile,
    outlocation
    ){
    dir.create(outlocation, recursive=T)
    
    command_for_system <- paste0(
        "unzip ", 
        zipfile,
        " -d ",
        outlocation
        )
    
    system(
        command_for_system, 
        wait=F
        )
}
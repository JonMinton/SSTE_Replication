# Jon Minton
# 

# Script for checking for and if necessary downloading HMD data
# NOTE : currently there are two sources of data
# 1) Older - from about 2010 when I first downloaded the data from the HMD
#    - the graphics were produced using this dataset, which may no longer be 
#      hosted on the HMD website
# 2) Newer - Data as of May 2014 - this should be as downloadable from the HMD
#    however it will also be hosted and downloaded from a dropbox location to make 
#    the file easier to locate

# 
# Write code to download and uppack, but only after checking that the file has not already been downloaded

if (OlderData){
    url_oldData <- "https://www.dropbox.com/s/frv4leanuym1kii/hmd_countries_Earlier.zip"
    print("Older Data")
    if (!file.exists("HMD_old.zip")){
        print("Older zipfile not found. Downloading")
        tmp <- httr::GET(url_oldData)
        writeBin(
            content(tmp, "raw"), 
            "HMD_old.zip"       
        )
    } else {
        print("Older zipfile found. Not downloading")
    }
    
    print("Older Data")
    if (!file.exists("Data/HMD/Old")){
        print("Unpackaged Directory not found. Unpacking")
        Unpack_HMD(
            zipfile="HMD_old.zip",
            outlocation="Data/HMD/Old/"
        )
    } else {
        print("Unpacked Directory found. Not unpacking")
    }

    # Also want to download and read from countrycodes data
    
    print("Country codes")
    if (!file.exists("Data/CountryCodes.xlsx")){
        print("CountryCodes.xlsx not found. Downloading")
        url <- "https://www.dropbox.com/s/9ap8qjxwnuk3b5g/CountryCodes.xlsx"    
        tmp <- httr::GET(url)
        writeBin(
            content(tmp, "raw"), 
            "Data/CountryCodes.xlsx"       
        )  
        rm(tmp)
        
    } else {
        print("CountryCodes.xlsx file found. Not downloading again")
    }
    
} else {
    url_newerData <- "https://www.dropbox.com/s/qcp8yk1f20lx6c0/hmd_statistics_12_05_2014.zip"
    
    
    print("Newer Data")
    if (!file.exists("HMD_newer.zip")){
        print("Newer zipfile not found. Downloading")
        tmp <- httr::GET(url_newerData)
        writeBin(
            content(tmp, "raw"), 
            "HMD_newer.zip"       
        )  
    } else{
        print("Newer zipfile found. Not downloading")
    }
    print("Newer Data")
    if (!file.exists("Data/HMD/Newer")){
        print("Unpacked Directory not found. Unpacking.")
        Unpack_HMD(
            zipfile="HMD_newer.zip",
            outlocation="Data/HMD/Newer/"
        )
    } else {
        print("Unpacked Directory found. Not unpacking.")
    }
    
    
}





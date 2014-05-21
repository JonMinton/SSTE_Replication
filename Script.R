## 21 / 5 / 2014
## Jon Minton
## Code for replicating results in SSTE article

# The data used were from the Human Mortality Database 
# www.mortality.org

# However, the most recent version of the dataset was not used. 
# I will host a copy of the version of the data I used, but also include code which will allow 
# the code to be run with the copy of the data currently available on the HMD website
# As of (21/5/2014)



# SessionInfo:
# R version 3.0.2 (2013-09-25)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# 
# locale:
#     [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    
# 
# attached base packages:
#     [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#     [1] tools_3.0.2



rm(list=ls())

require(RCurl)
require(repmis)
require(httr)
require(digest)
require(devtools)

source("Scripts/Functions.R") # Some issues with the function download at the moment - inconsistency - worked first time 
 # But not subsequent times

# Write code to download and uppack, but only after checking that the file has not already been downloaded

url_oldData <- "https://www.dropbox.com/s/frv4leanuym1kii/hmd_countries_Earlier.zip"
url_newerData <- "https://www.dropbox.com/s/qcp8yk1f20lx6c0/hmd_statistics_12_05_2014.zip"

download_file_url(
    url=url_oldData,
    outfile="HMD_old.zip"
    )

Unpack_HMD(
    zipfile="HMD_old.zip",
    outlocation="Data/HMD/Old/"
    )


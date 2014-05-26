## 26 / 5 / 2014
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


# Main stages:

# 1) Add correct dependencies (packages)
# 2) Read in source file
# 3) Check for and download HMD
# 4) Check for and produce Rdata
# 5) Run analyses 


rm(list=ls())

require(RCurl)
require(repmis)
require(httr)
require(digest)
require(devtools)
require(lattice)
require(downloader) 
require(xlsx)

OlderData=TRUE # Change this to false to use more recent
# Data from the HMD, or keep as TRUE for full reproducibility of results


source("Scripts/Functions.R") 
source("Scripts/Manage_Prerequisites.R")





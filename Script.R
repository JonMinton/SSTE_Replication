##  7 / 6/ 2014
## 27 / 5 / 2014
## 26 / 5 / 2014
## 21 / 5 / 2014

## Jon Minton
######################################################################################################
## CODE FOR REPLICATING FIGURES IN 
# Minton 2014, "Real Geographies and Virtual Landscapes: Exploring the Influence of 
# place and space on mortality Lexis surfaces using shaded contour maps", SSTE
# http://www.sciencedirect.com/science/article/pii/S1877584514000173

#######################################################################################################
# DATA 
# The data used were from the Human Mortality Database 
# www.mortality.org

# However, I used a slightly older version of the dataset (circa 2010), which has a slightly different 
# Data structure. 

# For replicability, I will host a zip file containing the version of the HMD I used. However, I have also
# included code which should allow more recent versions of the HMD to be used as well. (As of 21/5/2014)

# I will host a copy of the version of the data I used, but also include code which will allow 
# the code to be run with the copy of the data currently available on the HMD website


########################################################################################################
# Process: 
#
# This script will do the following:
# 1) Load required packages (use install.packages the first time)
# 2) Load various bespoke functions used for managing the data
# 3) Load the HMD data from a URL
# 4) Unpack the HMD data
# 5) Produce a range of HMD data list objects and save these in Derived_Data_Older.RData
# 6) Produce the figures in the SSTE article using the Derived Data objects 

# Given the derived data, it should be relatively straightforward to produce hundreds of additional visualisations
# Please contact me for additional code to automate the production of images. 

# For any queries please contact me
# Jonathan.minton@glasgow.ac.uk
# nate.minton@gmail.com

##############################################################################################################
# For any updates or corrections to this script please visit the GitHub repo:
# https://github.com/JonMinton/SSTE_Replication

#################################################################################################################

rm(list=ls())

require(RCurl)
require(repmis)
require(httr)
require(digest)
require(devtools)
require(lattice)
require(latticeExtra)
require(downloader) 
require(xlsx)
require(rgl)
require(tcltk)

Run_3D_Vis = TRUE

OlderData=TRUE # Change this to false to use more recent
# Data from the HMD, or keep as TRUE for full reproducibility of results
Replicate_Figures = FALSE

source("Scripts/Functions.R") 
source("Scripts/Manage_Prerequisites.R")


if (Replicate_Figures){
  # Figures to replicate
  source("Scripts/Make_Figures.R")  
}

#debug(Make_3D_Plot.UI)
if (Run_3D_Vis){
  tmp <- Make_3D_Plot.UI(DeathRates)
}



## Session Info:

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
# other attached packages:
#     [1] xlsx_0.5.5      xlsxjars_0.6.0  rJava_0.9-6     lattice_0.20-24 devtools_1.5    digest_0.6.4    httr_0.3        repmis_0.2.9   
# [9] RCurl_1.95-4.1  bitops_1.0-6   
# 
# loaded via a namespace (and not attached):
#     [1] evaluate_0.5.1    grid_3.0.2        memoise_0.2.1     parallel_3.0.2    plyr_1.8          R.cache_0.9.0     R.methodsS3_1.6.1
# [8] R.oo_1.17.0       R.utils_1.29.8    stringr_0.6.2     tools_3.0.2       whisker_0.3-2    

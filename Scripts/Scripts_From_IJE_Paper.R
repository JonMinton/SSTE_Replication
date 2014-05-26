# Run this before everything else...
## Code excluding functions

rm(list=ls())
current.wd <- "X:/Manuscripts/05 Accepted/1918 Cohort/1918/main/Data/" 
setwd(current.wd)
source("Functions.r")



###################################################################################################

# Data storage and management


# 13 July 2011

# Set wd


setwd(current.wd)

Country.Codes <- read.csv("hmd_countries/CountryCodes.csv", colClasses="character")
head(Country.Codes)

# directories for each country

country.Directories <- paste("hmd_countries/", Country.Codes[,1], "/STATS/", sep="")
# n.b. be sure to use 'paste' and not 'cat'

# Population

Populations <- Deaths <- Life.Expectancies <- vector("list", length(country.Directories))
names(Populations) <- names(Deaths) <- names(Life.Expectancies) <- Country.Codes[,"Country.Code"]
# one for each listed country

for (i in 1:length(country.Directories)){
  Populations[[i]] <- read.table(file=paste(country.Directories[i], "Population.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
  Deaths[[i]] <- read.table(file=paste(country.Directories[i], "Deaths_1x1.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
  Life.Expectancies[[i]] <- read.table(file=paste(country.Directories[i], "E0per.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
}

Populations.numeric <- lapply(Populations, function(x) MakeNumeric(x, T))
Deaths.numeric <- lapply(Deaths, function(x) MakeNumeric(x, T))

save(Populations, Populations.numeric, Deaths, Deaths.numeric, Life.Expectancies, Country.Codes, file="ComingAndGoing.rData")

################################################################################
################################################################################
Last.LE <- data.frame(Country=Country.Codes[,1], Year = 0, Male=0, Female=0, Total=0)
for (i in 1:length(country.Directories)){
  this.ds <- Life.Expectancies[[i]]
  last.year <- as.numeric(max(this.ds[,"Year"]))
  Last.LE[i,"Year"] <- last.year
  Last.LE[i,c("Female", "Male", "Total")] <- as.numeric(this.ds[dim(this.ds)[1],c("Female", "Male", "Total")])
}
Last.LE <- data.frame(Last.LE, Country.Name=Country.Codes$Country.Name)
Last.LE[order(Last.LE[,"Total"], decreasing=T),]

LE.2007 <- data.frame(Country=Country.Codes[,1], Year = 0, Male=0, Female=0, Total=0)
for (i in 1:length(country.Directories)){
  this.ds <- Life.Expectancies[[i]]
  LE.2007[i,"Year"] <- 2007
  LE.2007[i,c("Female", "Male", "Total")] <- as.numeric(this.ds[this.ds$Year=="2007",c("Female", "Male", "Total")])
}

LE.2007 <- data.frame(LE.2007, Country.Name=Country.Codes$Country.Name)
LE.2007[order(LE.2007[,"Total"], decreasing=T),]

################################################################################
################################################################################
# Calculate death rate densities
# Find expected values
# Find variances


setwd("D:/work/EpiTrans/Data/")
load("ComingAndGoing.rData")

# LOAD THE TWO FUNCTIONS

DeathRates <- DeathRates.EV <- Deaths.EV <- Deaths.EVexcInfants <- vector("list", length(names(Populations)))
names(DeathRates) <- names(DeathRates.EV) <- names(Deaths.EV) <- names(Deaths.EVexcInfants) <- names(Populations)

for (i in 1:length(names(Populations))){
  #  print(i)
  this.country <- names(DeathRates)[i]
  DeathRates[[this.country]] <- findDeathRates(Pop=Populations[[this.country]], Deaths=Deaths[[this.country]])
  DeathRates.EV[[this.country]] <- findMeanVar(DeathRates[[this.country]])
  Deaths.EV[[this.country]] <- findMeanVar(Deaths[[this.country]])
  Deaths.EVexcInfants[[this.country]] <- findMeanVar(Deaths[[this.country]], excChild=T)
  
}


save(DeathRates, DeathRates.EV, Deaths.EV, Deaths.EVexcInfants, file="DerivedData.rData")


#############################################################################################
# CONTOUR PLOTS #
#############################################################################################
rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)


for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- subset(this.ds, Age < 91) 
  
  tiff(paste("contourPlots/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlots/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlots/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlots/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlots/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlots/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
}


#############################################################################################
# CONTOUR PLOTS # - red heat maps
#############################################################################################
rm(list=ls())
current.wd <- "X:/Manuscripts/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)


for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- subset(this.ds, Age < 81) 

  tiff(paste("contourPlotsHEAT/males/10/DR_Male_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/males/20/DR_Male_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()

  tiff(paste("contourPlotsHEAT/females/10/DR_Female_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/females/20/DR_Female_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  
  tiff(paste("contourPlotsHEAT/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()

  tiff(paste("contourPlotsHEAT/total/10/DR_Total_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/total/20/DR_Total_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  
  tiff(paste("contourPlotsHEAT/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()

  tiff(paste("contourPlotsHEAT/dif/10/DR_Dif_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(Male - Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Difference", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/dif/20/DR_Dif_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(Male - Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Difference", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsHEAT/dif/50/DR_Dif_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(Male - Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Difference", sep="")))                   
  dev.off()

  tiff(paste("contourPlotsHEAT/dif/100/DR_Dif_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(Male - Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Difference", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/males/10/LogDR_Male_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/males/20/LogDR_Male_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/males/50/LogDR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/males/100/LogDR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()

  tiff(paste("ContourPlotsLog/females/10/LogDR_Female_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/females/20/LogDR_Female_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/females/50/LogDR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/females/100/LogDR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()

  tiff(paste("ContourPlotsLog/total/10/LogDR_Total_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Total) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/total/20/LogDR_Total_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Total) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/total/50/LogDR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Total) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/total/100/LogDR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(log(Total) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()

  tiff(paste("ContourPlotsLog/dif/10/LogDR_Dif_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(log(Male)  - log(Female))~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/dif/20/LogDR_Dif_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(log(Male) - log(Female)) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/dif/50/LogDR_Dif_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(log(Male)  - log(Female))~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("ContourPlotsLog/dif/100/LogDR_Dif_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(I(log(Male) - log(Female)) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
}



#############################################################################################
# CONTOUR PLOTS # - greyscale maps
#############################################################################################
rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)

gs.50 <- rev(grey(seq(0,1, by=0.02)))
gs.100 <- rev(grey(seq(0, 1, by=0.01)))


for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- subset(this.ds, Age < 91) 
  
  
  
  tiff(paste("contourPlotsCOL/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.50,at=quantile(this.ds$Male, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsCOL/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.100, at=quantile(this.ds$Male, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsCOL/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.50, at=quantile(this.ds$Female, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsCOL/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.100, at=quantile(this.ds$Female, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsCOL/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.50, at=quantile(this.ds$Total, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("contourPlotsCOL/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, contour=F, region=T, col.regions=gs.100, at=quantile(this.ds$Total, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
}


#############################################################################################
# CONTOUR PLOTS of differences in age-specific mortality rates year on year#
#############################################################################################
rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)


for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- GetAgeRateDif(this.ds, maxage=90)
  
  tiff(paste("DcontourPlots/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("DcontourPlots/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("DcontourPlots/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("DcontourPlots/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("DcontourPlots/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("DcontourPlots/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
}

# want to find out range of values for change, and (perhaps) convert values to 
# SDs from mean

for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- GetAgeRateDif(this.ds, maxage=90)
  
  this.ds$Female <- Standardise(this.ds$Female)
  this.ds$Male <- Standardise(this.ds$Male)
  this.ds$Total <- Standardise(this.ds$Total)
  
  tiff(paste("DcontourPlots/males/std/DR_Male_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2),  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  

  tiff(paste("DcontourPlots/females/std/DR_Female_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
  tiff(paste("DcontourPlots/total/std/DR_Total_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), ,  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
}

for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- GetAgeRateDif(this.ds, maxage=90)
  
  this.ds$Female <- Standardise(this.ds$Female)
  this.ds$Male <- Standardise(this.ds$Male)
  this.ds$Total <- Standardise(this.ds$Total)
  
  tiff(paste("DcontourPlots/males/stdlvl/DR_Male_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(levelplot(Male ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2),  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
  tiff(paste("DcontourPlots/females/stdlvl/DR_Female_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(levelplot(Female ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
  tiff(paste("DcontourPlots/total/stdlvl/DR_Total_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(levelplot(Total ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), ,  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
}

for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- GetAgeRateDif(this.ds, maxage=90)
  
  this.ds$Female <- Standardise(this.ds$Female)
  this.ds$Male <- Standardise(this.ds$Male)
  this.ds$Total <- Standardise(this.ds$Total)
  
  tiff(paste("DcontourPlots/males/std/DR_Male_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2),  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
  tiff(paste("DcontourPlots/females/std/DR_Female_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
  tiff(paste("DcontourPlots/total/std/DR_Total_Std_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, at=c(-2, -1.5, -1, 1, 1.5, 2), ,  main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()  
  
}


#############################################################################################
## Contour plots of cohort-specific mortality rates year on year 
# As requested by reviewers
#############################################################################################

rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")

load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)


for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- getCohortDeathRates(this.ds)
  
  tiff(paste("CcontourPlots/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
}

#############################################################################################
## Contour plots of cohort-specific mortality rates year on year 
# As requested by reviewers - greyscale
#############################################################################################

rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")

load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)

gs.50 <- rev(grey(seq(0,1, by=0.02)))
gs.100 <- rev(grey(seq(0, 1, by=0.01)))



for (i in 1:dim(Country.Codes)[1]){
  this.ds <- DeathRates[[Country.Codes[i,1]]]
  this.ds <- getCohortDeathRates(this.ds)
  
  tiff(paste("CcontourPlotsCOL/males/50/DR_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, contour=F, region=T, col.regions=gs.50, data=this.ds, at=quantile(this.ds$Male, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlotsCOL/males/100/DR_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, contour=F, region=T, col.regions=gs.100, data=this.ds, at=quantile(this.ds$Male, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlots/females/50/DR_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, contour=F, region=T, col.regions=gs.50, data=this.ds, at=quantile(this.ds$Female, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlotsCOL/females/100/DR_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, contour=F, region=T, col.regions=gs.100, data=this.ds, at=quantile(this.ds$Female, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlotsCOL/total/50/DR_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, contour=F, region=T, col.regions=gs.50, data=this.ds, at=quantile(this.ds$Total, probs=seq(0.02, 1, by=0.02), na.rm=T), main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("CcontourPlotsCOL/total/100/DR_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, contour=F, region=T, col.regions=gs.100, data=this.ds,at=quantile(this.ds$Total, probs=seq(0.01, 1, by=0.01), na.rm=T), main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
}


#############################################################################################
# Construct mean/var graphs
#############################################################################################
rm(list=ls())
current.wd <- "X:/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")

# INequality relationships

Data <- read.csv("InequalityMatched.csv")

codes <- names(Data)[-1]
Identifiers <- as.data.frame(strsplit(codes, "\\."))
Identifiers <- t(Identifiers)
rownames(Identifiers) <- 1:length(rownames(Identifiers))
colnames(Identifiers) <- c("Country", "Statistic")

#unique(Identifiers[,1])
#unique(Identifiers[,2])

LongData <- expand.grid(year=Data$Year, country=unique(Identifiers[,1]), statistic=unique(Identifiers[,2]))
LongData <- data.frame(LongData, value=NA)

for (i in 1:dim(Identifiers)[1]){
  LongData[LongData$country==Identifiers[i,"Country"] & LongData$statistic==Identifiers[i,"Statistic"], "value"] <- Data[,i+1]
}


for (i in 1:dim(Identifiers)[1]){
  for (j in 1:2){
    # j == 1 : male
    # j == 2 : female
    for (k in 1:2){
      # k == 1: including
      # k == 2: excluding
      
      if (j==1){
        titletext.sex <- "(Males)"
        filename.sex <- "M"
        filepath.sex <- "males/"
      }
      if (j==2){
        titletext.sex <- "(Females)"
        filename.sex <- "F"
        filepath.sex <- "females/"
      }
      
      if(k==1){
        titletext.incexc <- "including"
        filename.incexc <- "inc"
        filepath.incexc <- "inc/"
      } 
      if(k==2){
        titletext.incexc <- "excluding"
        filename.incexc <- "exc"
        filepath.incexc <- "exc/"
      }
      
      filename.country <- Identifiers[i,"Country"]
      titletext.country <- Country.Codes$Pretty.Country.Name[Country.Codes$Country.Code==Identifiers[i,"Country"]]
      
      filename.oneten <- Identifiers[i,"Statistic"]
      titletext.oneten <- ifelse(filename.oneten=="One", "1%", "10%")
      filepath.oneten <- ifelse(filename.oneten=="One", "One/", "Ten/")
      
      dta.y1 <- subset(LongData, country==filename.country & statistic==filename.oneten)
      
      filename <- paste(
        "y2Plots/",
        filepath.sex,
        filepath.oneten,
        filepath.incexc,
        filename.sex, "_", 
        filename.oneten, "_",
        filename.incexc, "_", 
        filename.country,
        ".tif", sep=""
      )
      
      tiff(file=filename, height=800, width=1200)
      
      par(mar=c(5,4,4,5)+0.1)
      
      if(Identifiers[i,2]=="One") {
        ylim <- c(0,30)
        titletext.oneten <- "1%"
      }
      
      if(Identifiers[i,2]=="Ten") {
        ylim <- c(0,80)
        titletext.oneten <- "10%"
      }  
      titletext <- paste(
        titletext.country, 
        ": % Wealth owned by Top ", 
        titletext.oneten, 
        " and variance in ages of death ",
        titletext.sex, " ",
        titletext.incexc, 
        " infants", 
        sep=""
      )
      
      plot(
        value ~ year, 
        data=dta.y1, 
        main=titletext, 
        type="l", 
        lwd=2, 
        lty="dashed", 
        ylim=ylim, 
        xlab="Year", 
        ylab="%"
      )
      
      par(new=TRUE)
      
      dta.y2 <- data.frame(year= 1875:2009, value=NA)
      if(k==1){ 
        temp2.ds <- Deaths.EV[[filename.country]] 
      }
      if(k==2){
        temp2.ds <- Deaths.EVexcInfants[[filename.country]]
      }
      
      temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
      if(j==1) {
        dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleVar
      } 
      if(j==2) {
        dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$FemaleVar
      }       
      
      plot(
        value ~ year, 
        data=dta.y2, 
        type="l", 
        xaxt="n", 
        yaxt="n", 
        xlab="", 
        ylab="", 
        log="y", 
        lwd=2, 
        ylim=c(100,1500)
      )
      axis(4)
      
      mtext(expression(paste("Variance (Year",s^2,")")),side=4,line=3)
      
      legend("bottomleft", cex=1.2, legend=c("Variance", "%"), lty=c("solid", "dashed"), lwd=c(2,2), bty="n")
      
      dev.off()
    }
  }  
}


##########################################################################################
# Mean-variance graphs ###################################################################
##########################################################################################
# code for two axis graphs
# now to produce mean/variance graphs

for (i in 1:dim(Country.Codes)[1]){
  for (j in 1:2){
    for (k in 1:2){
      # j 1=male; 2=female
      # k 1=inc ; 2=exc
      if (j==1){
        titletext.sex <- "(Males)"
        filename.sex <- "M"
        filepath.sex <- "males/"
      }
      if (j==2){
        titletext.sex <- "(Females)"
        filename.sex <- "F"
        filepath.sex <- "females/"
      }
      
      if(k==1){
        titletext.incexc <- "including"
        filename.incexc <- "inc"
        filepath.incexc <- "inc/"
      } 
      if(k==2){
        titletext.incexc <- "excluding"
        filename.incexc <- "exc"
        filepath.incexc <- "exc/"
      }
      
      filename.country <-Country.Codes[i, "Country.Code"]
      titletext.country <- Country.Codes[i, "Pretty.Country.Name"]
      
      
      
      filename <- paste(
        "meanVarPlots/",
        filepath.sex,
        filepath.incexc,
        filename.sex, "_", 
        filename.incexc, "_", 
        filename.country,
        ".tif", sep=""
      )
      
      tiff(file=filename, height=800, width=1200)
      
      par(mar=c(5,4,4,5)+0.1)
      
      titletext <- paste(
        titletext.country, 
        ": mean and variance in ages of death ",
        titletext.sex, " ",
        titletext.incexc, 
        " children", 
        sep=""
      )
      
      
      if(k==1){
        dta.y1 <- Deaths.EV[[filename.country]]
        dta.y2 <- Deaths.EV[[filename.country]]
      }
      
      if(k==2){
        dta.y1 <- Deaths.EVexcInfants[[filename.country]]
        dta.y2 <- Deaths.EVexcInfants[[filename.country]]
      }
      
      if(j==1){
        names(dta.y1)[c(1,2)] <- c("year", "value")
        names(dta.y2)[c(1,3)] <- c("year", "value")
      }
      
      if(j==2){
        names(dta.y1)[c(1,4)] <- c("year", "value")
        names(dta.y2)[c(1,5)] <- c("year", "value")
      }
      
      plot(
        value ~ year, 
        data=dta.y1, 
        main=titletext, 
        type="l", 
        lwd=2, 
        lty="solid", 
        ylim=c(0,100), 
        xlab="Year", 
        ylab="Mean (Years)"
      )
      abline(v=c(1914,1918, 1939, 1945), lty="dashed")
      
      par(new=TRUE)
      
      
      plot(
        value ~ year, 
        data=dta.y2, 
        type="l", 
        xaxt="n", 
        yaxt="n", 
        xlab="", 
        ylab="", 
        lty="dashed",
        #      log="y", 
        lwd=2, 
        ylim=c(100,1200)
      )
      axis(4)
      
      mtext(expression(paste("Variance (Year",s^2,")")),side=4,line=3)
      
      legend("bottomleft", cex=1.2, legend=c("Mean", "Variance"), lty=c("solid", "dashed"), lwd=c(2,2), bty="n")
      
      dev.off()      
      
      
    }
    
  }
  
}


#################################################

# Calculate Mean-Variance correlations

r.male.exc <- r.male.inc <- r.female.exc <- r.female.inc <- data.frame(country=Country.Codes$Country.Code, pearson=NA, spearman=NA)

for (i in 1:dim(Country.Codes)[1]){
  tmp <- cor(Deaths.EV[[i]][,c("MaleMean", "MaleVar")], use="complete")
  r.male.inc[i,"pearson"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EV[[i]][,c("MaleMean", "MaleVar")], use="complete", method="spearman")
  r.male.inc[i, "spearman"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EV[[i]][,c("FemaleMean", "FemaleVar")], use="complete")
  r.female.inc[i,"pearson"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EV[[i]][,c("FemaleMean", "FemaleVar")], use="complete", method="spearman")
  r.female.inc[i,"spearman"] <- tmp[lower.tri(tmp)]
  
  
  
  tmp <- cor(Deaths.EVexcInfants[[i]][,c("MaleMean", "MaleVar")], use="complete")
  r.male.exc[i,"pearson"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EVexcInfants[[i]][,c("MaleMean", "MaleVar")], use="complete", method="spearman")
  r.male.exc[i, "spearman"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EVexcInfants[[i]][,c("FemaleMean", "FemaleVar")], use="complete")
  r.female.exc[i,"pearson"] <- tmp[lower.tri(tmp)]
  
  tmp <- cor(Deaths.EVexcInfants[[i]][,c("FemaleMean", "FemaleVar")], use="complete", method="spearman")
  r.female.exc[i,"spearman"] <- tmp[lower.tri(tmp)]
  
}

##########################################################################################
# Life expectancies, for each country, for males and females
##########################################################################################

setwd(current.wd)
load("DerivedData.rData")
load("ComingAndGoing.rData")


for (i in 1:dim(Country.Codes)[1]){
  
  filename.country <-Country.Codes[i, "Country.Code"]
  titletext.country <- Country.Codes[i, "Pretty.Country.Name"]
  
  
  
  filename <- paste(
    "lifeExpectancies/LE_",
    filename.country,
    ".tif", sep=""
  )
  
  tiff(file=filename, height=800, width=1200)
  
  
  titletext <- paste(
    titletext.country, 
    ": Life expectancies at birth",
    sep=""
  )
  
  
  plot(
    Male ~ Year, 
    data=Life.Expectancies[[i]], 
    main=titletext, 
    type="l", 
    lwd=2, 
    lty="solid", 
    ylim=c(0,100), 
    xlab="Year", 
    ylab="Life expectancy at birth (Years)"
  )
  
  lines(
    Female ~ Year,
    data=Life.Expectancies[[i]],
    lwd= 2,
    lty="dashed"
  )
  
  legend(
    "bottomright",
    legend=c("Males", "Females"),
    lwd=c(2,2),
    lty=c("solid", "dashed")
  )
  dev.off()      
  
  
}


#################################################################################################### 
# Three dimensional plots ##########################################################################
####################################################################################################

# 28 September 2012

# Now to look again at how to visualise these data in 3d
# Now a 3-D display of the attractor that is rotatable
rm(list=ls())

current.wd <- "X:/1918 Cohort/1918/main/Data"

setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")





# England & Wales, Males
Make3dPlot(DeathRates,country="GBRTENW", sex="male")
play3d(spin3d())

# England & Wales, Females
Make3dPlot(DeathRates,country="GBRTENW", sex="female")
play3d(spin3d())



### Australia
# Males
Make3dPlot(DeathRates,country="AUS", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="AUS", sex="female")
play3d(spin3d())


### Austria
# Males
Make3dPlot(DeathRates,country="AUT", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="AUT", sex="female", impute=T)
play3d(spin3d())

### Belgium
# Males
Make3dPlot(DeathRates,country="BEL", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="BEL", sex="female")
play3d(spin3d())

### Bulgaria
# Males
Make3dPlot(DeathRates,country="BGR", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="BGR", sex="female")
play3d(spin3d())

### Belarus
# Males
Make3dPlot(DeathRates,country="BLR", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="BLR", sex="female")
play3d(spin3d())


### Canada
# Males
Make3dPlot(DeathRates,country="CAN", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="CAN", sex="female")
play3d(spin3d())

### Switzerland
# Males
Make3dPlot(DeathRates,country="CHE", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="CHE", sex="female", impute=T)
play3d(spin3d())


### Chile
# Males
Make3dPlot(DeathRates,country="CHL", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="CHL", sex="female")
play3d(spin3d())


### Czech Republic
# Males
Make3dPlot(DeathRates,country="CZE", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="CHL", sex="female")
play3d(spin3d())

### East Germany
# Males
Make3dPlot(DeathRates,country="DEUTE", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DEUTE", sex="female")
play3d(spin3d())


### West Germany (Federal Republic of Germany)
# Males
Make3dPlot(DeathRates,country="DEUTFRG", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DEUTFRG", sex="female")
play3d(spin3d())

### East Germany (German Democratic Republic)
# Males
Make3dPlot(DeathRates,country="DEUTGDR", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DEUTGDR", sex="female")
play3d(spin3d())

### Germany
# Males
Make3dPlot(DeathRates,country="DEUTNP", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DEUTNP", sex="female")
play3d(spin3d())

### West Germany
# Males
Make3dPlot(DeathRates,country="DEUTW", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DEUTW", sex="female")
play3d(spin3d())

### Denmark
# Males
Make3dPlot(DeathRates,country="DNK", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="DNK", sex="female", impute=T)
play3d(spin3d())

### Spain
# Males
Make3dPlot(DeathRates,country="ESP", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="ESP", sex="female")
play3d(spin3d())


### Estonia
# Males
Make3dPlot(DeathRates,country="EST", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="EST", sex="female", impute=T)
play3d(spin3d())


### Finland
# Males
Make3dPlot(DeathRates,country="FIN", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="FIN", sex="female", impute=T)
play3d(spin3d())


### France, Civilian
# Males
Make3dPlot(DeathRates,country="FRACNP", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="FRACNP", sex="female")
play3d(spin3d())


### France, Civilian
# Males
Make3dPlot(DeathRates,country="FRATNP", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="FRATNP", sex="female")
play3d(spin3d())


### Northern Ireland
# Males
Make3dPlot(DeathRates,country="GBR_NIR", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="GBR_NIR", sex="female", impute=T)
play3d(spin3d())


### United Kingdom
# Males
Make3dPlot(DeathRates,country="GBR_NP", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="GBR_NP", sex="female")
play3d(spin3d())


### Scotland
# Males
Make3dPlot(DeathRates,country="GBR_SCO", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="GBR_SCO", sex="female", impute=T)
play3d(spin3d())


### England & Wales (Civilian)
# Males
Make3dPlot(DeathRates,country="GBRCENW", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="GBRCENW", sex="female")
play3d(spin3d())


### England & Wales (Total)
# Males
Make3dPlot(DeathRates,country="GBRTENW", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="GBRTENW", sex="female")
play3d(spin3d())


### Hungary
# Males
Make3dPlot(DeathRates,country="HUN", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="HUN", sex="female")
play3d(spin3d())


### Ireland
# Males
Make3dPlot(DeathRates,country="IRL", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="IRL", sex="female", impute=T)
play3d(spin3d())


### Iceland
# Males
Make3dPlot(DeathRates,country="ISL", sex="male", impute=T, ages=0:75, repeat.it=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="ISL", sex="female", impute=T, ages=0:75, repeat.it=T)
play3d(spin3d())


### Israel
# Males
Make3dPlot(DeathRates,country="ISR", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="ISR", sex="female")
play3d(spin3d())


### Italy
# Males
Make3dPlot(DeathRates,country="ITA", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="ITA", sex="female")
play3d(spin3d())


### Japan
# Males
Make3dPlot(DeathRates,country="JPN", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="JPN", sex="female")
play3d(spin3d())



### Lithuania
# Males
Make3dPlot(DeathRates,country="LTU", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="LTU", sex="female", impute=T)
play3d(spin3d())


### Luxembourg
# Males
Make3dPlot(DeathRates,country="LUX", sex="male", impute=T, repeat.it=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="LUX", sex="female", impute=T, repeat.it=T)
play3d(spin3d())


### Latvia
# Males
Make3dPlot(DeathRates,country="LVA", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="LVA", sex="female", impute=T)
play3d(spin3d())


### Netherlands
# Males
Make3dPlot(DeathRates,country="NLD", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="NLD", sex="female")
play3d(spin3d())


### Norway
# Males
Make3dPlot(DeathRates,country="NOR", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="NOR", sex="female", impute=T)
play3d(spin3d())


### New Zealand, Maori
# Males
Make3dPlot(DeathRates,country="NZL_MA", sex="male", impute=T, repeat.it=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="NZL_MA", sex="female", impute=T, repeat.it=T)
play3d(spin3d())


### New Zealand, Non Maori
# Males
Make3dPlot(DeathRates,country="NZL_NM", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="NZL_NM", sex="female", impute=T)
play3d(spin3d())


### New Zealand
# Males
Make3dPlot(DeathRates,country="NZL_NP", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="NZL_NP", sex="female", impute=T)

play3d(spin3d())


### Poland
# Males
Make3dPlot(DeathRates,country="POL", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="POL", sex="female")
play3d(spin3d())


### Portugal
# Males
Make3dPlot(DeathRates,country="PRT", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="PRT", sex="female")
play3d(spin3d())


### Russia
# Males
Make3dPlot(DeathRates,country="RUS", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="RUS", sex="female")
play3d(spin3d())


### Slovakia
# Males
Make3dPlot(DeathRates,country="SVK", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="SVK", sex="female", impute=T)
play3d(spin3d())


### Slovenia
# Males
Make3dPlot(DeathRates,country="SVN", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="SVN", sex="female", impute=T)
play3d(spin3d())


### Sweden
# Males
Make3dPlot(DeathRates,country="SWE", sex="male", impute=T)
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="SWE", sex="female", impute=T)
play3d(spin3d())


### Taiwan
# Males
Make3dPlot(DeathRates,country="TWN", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="TWN", sex="female")
play3d(spin3d())


### Ukraine
# Males
Make3dPlot(DeathRates,country="UKR", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="UKR", sex="female")
play3d(spin3d())

### United States
# Males
Make3dPlot(DeathRates,country="USA", sex="male")
play3d(spin3d())

# Females
Make3dPlot(DeathRates,country="USA", sex="female")
play3d(spin3d())

Make3dPlot(DeathRates,country="USA", sex="female")

####################################################################################################
########################## CALCULATE COMMON FACTORS ################################################
####################################################################################################



All.Commons <- vector("list", 48)
names(All.Commons) <- names(Deaths.EV)

for (i in 1:48){
  tmp <- CalcCommon(Deaths.EV[[i]], Deaths.EVexcInfants[[i]])
  
  All.Commons[[i]] <- data.frame(year=tmp$years, common=tmp$common)
}

min.year <- min(sapply(All.Commons, function(x) min(x$year)))
max.year <- max(sapply(All.Commons, function(x) max(x$year)))

plot(NA, type="n", ylim=c(-5, 5), xlim=c(min.year, max.year), xlab="year", ylab="Common factor")

for (i in 1:48){  
  points(common ~ year, data=All.Commons[[i]])
  cat(names(All.Commons)[[i]], "\n")
  browser()
}


# countries demonstrating the pattern most strongly:

# CHE
# DNK
# ESP
# FIN
# FRACNP
# FRATNP
# GBR_SCO
# GBRCENW
# GBRTENW
# ISL
# ITA
# NLD
# NOR

special.countries <- c("CHE", "DNK", "ESP", "FIN", 
                       "FRATNP", "GBR_SCO", "GBRTENW", "ISL", "ITA", "NLD", "NOR")

n <- length(special.countries)

plot(NA, type="n", ylim=c(-2, 2), xlim=c(1850, max.year), xlab="year", ylab="Common factor")

for (i in 1:n){  
  dta <- All.Commons[[special.countries[i] ]]
  points(common ~ year, data=dta)
  x <- dta$year 
  y <- dta$common
  lines(lowess(x, y), lwd=2)
  cat(special.countries[i], "\n")
  browser()
}

##########################################################################################################
###########INVESTIGATING HAZARD RATE ASSOCIATED WITH 1918 COHORT #########################################
##########################################################################################################

rm(list=ls())

current.wd <- "X:/Manuscripts/1918 Cohort/1918/main/Data" 

setwd(current.wd)
load("DerivedData.rData")
load("ComingAndGoing.rData")

source("Functions.r")
# Use the contents of DeathRates
# Calculate cohort birthyear by subtracting Age from Year

# Return age&gender-specific deathrates for a given cohort year


# Now, I want to know,
#   compared with the most recent year
#   which year has the highest age-gender-specific HR? 

# Say, compared with 1990?

Dta <- DeathRates[["GBRTENW"]]

tmp.1909 <- GetCohortDeathRates(Dta, cohortYear=1909)
tmp.1910 <- GetCohortDeathRates(Dta, cohortYear=1910)
tmp.1911 <- GetCohortDeathRates(Dta, cohortYear=1911)
tmp.1912 <- GetCohortDeathRates(Dta, cohortYear=1912)
tmp.1913 <- GetCohortDeathRates(Dta, cohortYear=1913)
tmp.1914 <- GetCohortDeathRates(Dta, cohortYear=1914)
tmp.1915 <- GetCohortDeathRates(Dta, cohortYear=1915)
tmp.1916 <- GetCohortDeathRates(Dta, cohortYear=1916)
tmp.1917 <- GetCohortDeathRates(Dta, cohortYear=1917)
tmp.1918 <- GetCohortDeathRates(Dta, cohortYear=1918)
tmp.1919 <- GetCohortDeathRates(Dta, cohortYear=1919)
tmp.1920 <- GetCohortDeathRates(Dta, cohortYear=1920)
tmp.1921 <- GetCohortDeathRates(Dta, cohortYear=1921)
tmp.1922 <- GetCohortDeathRates(Dta, cohortYear=1922)
tmp.1923 <- GetCohortDeathRates(Dta, cohortYear=1923)
tmp.1924 <- GetCohortDeathRates(Dta, cohortYear=1924)
tmp.1925 <- GetCohortDeathRates(Dta, cohortYear=1925)
tmp.1926 <- GetCohortDeathRates(Dta, cohortYear=1926)
tmp.1927 <- GetCohortDeathRates(Dta, cohortYear=1927)
tmp.1928 <- GetCohortDeathRates(Dta, cohortYear=1928)
tmp.1929 <- GetCohortDeathRates(Dta, cohortYear=1929)
tmp.1930 <- GetCohortDeathRates(Dta, cohortYear=1930)



plot(NA, ylim=c(-8, 0), xlim=c(0,80), xlab="age", ylab="rate")

Grabber <- list(
  y1910=tmp.1910,
  y1911=tmp.1911,
  y1912=tmp.1912,
  y1913=tmp.1913,
  y1914=tmp.1914,
  y1915=tmp.1915,
  y1916=tmp.1916,
  y1917=tmp.1917,
  y1918=tmp.1918,
  y1919=tmp.1919,
  y1920=tmp.1920,
  y1921=tmp.1921,
  y1922=tmp.1922,
  y1923=tmp.1923,
  y1924=tmp.1924,
  y1925=tmp.1925
)

cols <- rainbow(length(Grabber))
for (i in 1:length(Grabber)){
  lines(log(rate) ~ age, data=Grabber[[i]], col=cols[i], lwd=ifelse(i==3, 2,1))
}


legend("bottomright", legend=c(1912, 1915, 1918, 1921, 1924), col=cols, lty="solid")

# let's look at age=5

modFits <- vector("list", length(unique(tmp$age)))

for (i in 1:length(unique(tmp$age))){
  this.age <- unique(tmp$age)[i]
  
  modFits[[i]] <- lm(value ~ year, data=age==this.age)
  
}


for (i in 1:length(years)){
  for (j in 1:length(ages)){
    tmp <- Our.Data[Our.Data$Age==ages[j] & Our.Data$Year==years[i],"Male"]
    if(length(tmp)==1){
      Reshaped.Male[i,j] <- tmp
    }
  }
}
dimnames(Reshaped.Male) <- list(years, ages)

# For a particular dataset, for a particular gender,
# I want to pull out the diagonals,

ExtractCohort <- function(Dta, birthYear){
  to.extract <- with(Dta, which(Year - Age ==birthYear))
  if(length(to.extract)==0) {return(NULL)}else{
    Output <- with(Dta,    data.frame(age=Age[to.extract], 
                                      male=Male[to.extract], 
                                      female=Female[to.extract], 
                                      total=Total[to.extract]
    )
    )
    return(Output)  
  }
}

plot(NA, ylim=c(-8, 0), xlim=c(0,100))

lines(log(male) ~ age, data=ExtractCohort(Our.Data, 1900))
lines(log(male) ~ age, data=ExtractCohort(Our.Data, 1910))
lines(log(male) ~ age, data=ExtractCohort(Our.Data, 1920))
lines(log(male) ~ age, data=ExtractCohort(Our.Data, 1930))
lines(log(male) ~ age, data=ExtractCohort(Our.Data, 1940))


########################################################################################################


######

i <- 1
filename.country <- Country.Codes[i, "Country.Code"]
temp2.ds <- Deaths.EV[[filename.country]]
dta.y2 <- data.frame(year= 1875:2009, value=NA)

temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleMean

plot(value ~ year, data=dta.y2, type="p", ylim=c(0,100))

for (i in 2:dim(Country.Codes)[1]){
  filename.country <- Country.Codes[i, "Country.Code"]
  temp2.ds <- Deaths.EV[[filename.country]]
  dta.y2 <- data.frame(year= 1875:2009, value=NA)
  
  temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
  dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleMean
  
  points(value ~ year, data=dta.y2)
}

##

i <- 1
filename.country <- Country.Codes[i, "Country.Code"]
temp2.ds <- Deaths.EV[[filename.country]]
dta.y2 <- data.frame(year= 1875:2009, value=NA)

temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleVar

plot(value ~ year, data=dta.y2, type="l", ylim=c(100,1200))

for (i in 2:dim(Country.Codes)[1]){
  filename.country <- Country.Codes[i, "Country.Code"]
  temp2.ds <- Deaths.EV[[filename.country]]
  dta.y2 <- data.frame(year= 1875:2009, value=NA)
  
  temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
  dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleVar
  
  lines(value ~ year, data=dta.y2)
}  

# for density plot

dta <- data.frame(year=rep(1875:2009, dim(Country.Codes)[1]), value=NA)
valStack <- c()

for (i in 1:dim(Country.Codes)[1]){
  dta.y2 <- data.frame(year= 1875:2009, value=NA)
  filename.country <- Country.Codes[i, "Country.Code"]
  temp2.ds <- Deaths.EV[[filename.country]]
  
  temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
  dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleMean
  valStack <- c(valStack, dta.y2$value)
}
dta$value <-valStack

plot(value ~ year, data=dta, type="p")

require(hexbin)  
plot(hexbin(x=dta$year, y=dta$value, xbins=75), type="p")
#####

dta <- data.frame(year=rep(1875:2009, dim(Country.Codes)[1]), value=NA)
valStack <- c()

for (i in 1:dim(Country.Codes)[1]){
  dta.y2 <- data.frame(year= 1875:2009, value=NA)
  filename.country <- Country.Codes[i, "Country.Code"]
  temp2.ds <- Deaths.EV[[filename.country]]
  
  temp2.ds <- subset(temp2.ds, Year > 1874 & Year < 2010)
  dta.y2$value[match(temp2.ds$Year, dta.y2$year)] <- temp2.ds$MaleVar
  valStack <- c(valStack, dta.y2$value)
}
dta$value <-valStack

require(hexbin)  
plot(hexbin(x=dta$year, y=dta$value, xbins=75), type="p")

####

# Death Rate Cohort graphs...

setwd(current.wd)

load("DerivedData.rData")
load("ComingAndGoing.rData")

N <- 47
ds <- DeathRates[[N]]

ageRange <- 0:110
yearRange <- unique(ds$Year) 

Cohort.Matrix <- matrix(NA, nrow=length(yearRange) , ncol=length(ageRange))
rownames(Cohort.Matrix) <- yearRange
colnames(Cohort.Matrix) <- ageRange

require(reshape)

ds.prep <- melt(ds[,c("Age", "Year", "Male")], id=c("Year", "Age"))
ds.prep <- cast(ds.prep, Year ~ Age)
ds.prep <- data.matrix(ds.prep[,-1])

for (i in 1:dim(Cohort.Matrix)[1]-1){
  tmp <- diag(ds.prep[i:nrow(ds.prep),])
  Cohort.Matrix[i,1:length(tmp)] <- tmp
}

plot(Cohort.Matrix[1,], type="l", log="y", xlim=c(0,90), main=names(DeathRates)[N])
for( i in 2:dim(Cohort.Matrix)[1]-1){lines(Cohort.Matrix[i,])}
lines(Cohort.Matrix["1920",], col="red", lwd=2)
lines(Cohort.Matrix["1915",], col="green", lwd=2)
lines(Cohort.Matrix["1925",], col="purple", lwd=2)
lines(Cohort.Matrix["1930",], col="blue", lwd=2)


##### Find earliest year

Country.Minyear <- data.frame(country=Country.Codes$Country.Code, minYear=NA, maxYear=NA)
Country.Minyear[,2] <- sapply(DeathRates, function (x) min(x$Year))
Country.Minyear[,3] <- sapply(DeathRates, function (x) max(x$Year))
Country.Minyear[order(Country.Minyear[,2]),]
Country.Minyear[order(Country.Minyear[,3]),]

# Total number of obvservations in each dataset?

Country.N <- data.frame(country=Country.Codes$Country.Code, lives=NA, deaths=NA)

Country.N[,2] <- sapply(Populations, function(x) (sum(x$Total, na.rm=T)))
Country.N[,3] <- sapply(Deaths, function(x) (sum(x$Total, na.rm=T)))

####

# density plot, England & Wales, 1900 and 2000

setwd(current.wd)

load("DerivedData.rData")
load("ComingAndGoing.rData")

Dataset <- DeathRates[["GBRTENW"]]

plot(Total ~ Age, data=subset(Dataset, Year==1900), type="l", xlim=c(0,90), ylab="Death Rate", ylim=c(0.0001, 1), log="y", lwd=2)
legend("bottomright", legend=c("1900", "2000"), lwd=c(2,2),lty=c("solid", "dashed"))
lines(Total ~ Age, data=subset(Dataset, Year==2000), lwd=2, lty="dashed")

# n.b. still (perhaps) need to work on formatting left axis

######################################################


# 16/6/2013

# aim to estimate the size of the 1918 effect by removing the data and imputing, then comparing imputed with 
# real observations 

rm(list=ls())

current.wd <- "X:/1918 Cohort/1918/main/Data"

setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

D <- GetValMat(DeathRates, "GBRTENW", "male")

# Let's remove the 1917 to 1920 cohort

D2 <- D

for (i in 1917:1922){
  for (j in 0:80){
    D2[as.character(j),as.character(i + j)] <- NA
  }
}


D3 <- exp(GraphImpute(D2, repeat.it=T)[["output"]])

Ddif <- D - D3 # real - imputed

Difs <- data.frame(age=0:80, y17=NA, y18=NA, y19=NA, y20=NA, y21=NA, y22=NA)

for (i in 0:80){
  Difs[i,"y17"] <- Ddif[as.character(i), as.character(1917 + i)]
  Difs[i,"y18"] <- Ddif[as.character(i), as.character(1918 + i)]
  Difs[i,"y19"] <- Ddif[as.character(i), as.character(1919 + i)]
  Difs[i,"y20"] <- Ddif[as.character(i), as.character(1920 + i)]
  Difs[i,"y21"] <- Ddif[as.character(i), as.character(1921 + i)]
  Difs[i,"y22"] <- Ddif[as.character(i), as.character(1922 + i)]
}

matplot(Difs[,1], Difs[-1], type="l", ylim=c(-0.01, 0.03), xlab="age", ylab="Diff in mortality rate")

# This implies it was the 1920 cohort who experienced the worst effects. 

###############################################################################################

rm(list=ls())

current.wd <- "X:/1918 Cohort/1918/main/Data"

setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

this.ds <- DeathRates[["GBRTENW"]]
this.ds <- subset(this.ds, subset=Age<81)
# Plot for blog: 
# how has the 0.01 mortality hurdle changed for males over time in England & Wales?
require(lattice)
require(latticeExtra)
jpeg("maleBlog.jpg", height=1000, width=1000, quality=100, res=100)

Part1 <- contourplot(Male ~ Year * Age, 
                     data=this.ds,
                     region=T, 
                     col.regions=rev(heat.colors(200)),
                     cuts=50, 
                     main="", 
                     labels=F,
                     lwd=2,
                     col="grey",
                     xlab=list(
                       label="Year",
                       cex=1.5),
                     ylab=list(
                       label="Age",
                       cex=1.5
                       ),
                     scales=list(
                       cex=1.5
                       ),
                     sep="")

Part2 <- contourplot(Male ~ Year * Age,
                     data=this.ds,
                     region=F,
                     labels=F,
                     lwd=2,
                     at=c(0, 0.01, 1),
                     panel=function(...){
                       panel.contourplot(...)
                       panel.segments(x0=c(1840, 1900, 2000, 1840), 
                                      x1=c(1900, 1900, 2000, 2000), 
                                      y0=c(37.1, 0, 0, 59.4), 
                                      y1=c(37.1, 37.1, 59.4, 59.4), 
                                      lty="dashed")
                     }                    
  )


Plot.Final <- Part1+ Part2 

print(Plot.Final)
dev.off()

# Now highlighting World Wars

this.ds <- DeathRates[["GBRTENW"]]
this.ds <- subset(this.ds, subset=Age<81)
# Plot for blog: 
# how has the 0.01 mortality hurdle changed for males over time in England & Wales?
require(lattice)
require(latticeExtra)
jpeg("WarMort.jpg", height=1000, width=1000, quality=100, res=100)

Part1 <- contourplot(Male ~ Year * Age, 
                     data=this.ds,
                     region=T, 
                     col.regions=rev(heat.colors(200)),
                     cuts=50, 
                     main="", 
                     labels=F,
                     col="grey",
                     xlab=list(
                       label="Year",
                       cex=1.5),
                     ylab=list(
                       label="Age",
                       cex=1.5
                     ),
                     scales=list(
                       cex=1.5
                     ),
                     sep="")

Part2 <- contourplot(Male ~ Year * Age,
                     data=subset(this.ds, subset=Age < 45 & Age > 13 & Year > 1913 & Year < 1921),
                     at=Part1$legend$right$args$key$at, 
                     main="", 
                     labels=F,
                     col="black",
                     panel=function(...){
                       panel.contourplot(...)
                       panel.segments(
                         x0=c(1914, 1914, 1920,  1914, 1939, 1939, 1946, 1939),
                         x1=c(1914, 1920, 1920,  1920, 1939, 1946, 1946, 1946),
                         y0=c(16,   16,   16,    44,   16, 16, 16, 44),
                         y1=c(44,   16,   44,    44,   44, 16, 44, 44),
                         lty="dashed"
                           )
                     }
                       
                       
)

Part3 <- contourplot(Male ~ Year * Age,
                     data=subset(this.ds, subset=Age < 45 & Age > 13 & Year > 1938 & Year < 1947),
                     at=Part1$legend$right$args$key$at, 
                     main="", 
                     col="black",
                     labels=F
)


Plot.Final <- Part1+ Part2 +Part3

print(Plot.Final)
dev.off()

jpeg("WarMortFem.jpg", height=1000, width=1000, quality=100, res=100)

Part1 <- contourplot(Female ~ Year * Age, 
                     data=this.ds,
                     region=T, 
                     col.regions=rev(heat.colors(200)),
                     cuts=50,
                     xlab=list(
                       label="Year",
                       cex=1.5),
                     ylab=list(
                       label="Age",
                       cex=1.5
                     ),
                     scales=list(
                       cex=1.5
                     ),
                  
                     main="", 
                     labels=F,
                     col="grey",
                     sep="")
that.ds <- this.ds
that.ds <- data.frame(that.ds, cohort=F)
that.ds$cohort[((that.ds$Year - that.ds$Age)  > 1916) & ((that.ds$Year - that.ds$Age) < 1922)] <- T


Part2 <- contourplot(Female ~ Year * Age,
                     data=subset(this.ds, subset=Age < 45 & Age > 13 & Year > 1915 & Year < 1921),
                     at=Part1$legend$right$args$key$at, 
                     main="", 
                     labels=F,
                     panel=function(...){
                       panel.contourplot(...)
                       panel.segments(
                         x0=c(1916, 1916, 1920,  1916, 1916, 1916, 1916, 1921, 1916, 1921),
                         x1=c(1916, 1920, 1920,  1920, 1916, 1920, 1920, 1920, 1997, 2002),
                         y0=c(13,   13,   13,    44,  -1,   -1, 10, -1, 0, 0),
                         y1=c(44,   13,   44,    44,  10,  -1, 10, 10, 80, 80),
                         lty="dashed"
                       )
                       
                       panel.segments(
                         x0=c(1939, 1939, 1946, 1939),
                         x1=c(1939, 1946, 1946, 1946),
                         y0=c(13, 13, 13, 44),
                         y1=c(44, 13, 44, 44),
                         lty="dashed",
                         col="grey"
                       )
                     }
)

Part3 <- contourplot(Female ~ Year * Age,
                     data=subset(this.ds, subset=Age < 10 & Year > 1915 & Year < 1921),
                     at=Part1$legend$right$args$key$at, 
                     main="", 
                     labels=F
)

Part4 <- contourplot(Female ~ Year * Age,
                     data=subset(that.ds, subset=cohort==T & Year > 1950),
                     at=Part1$legend$right$args$key$at, 
                     main="", 
                     labels=F

)



Plot.Final <- Part1+ Part2 +Part3 + Part4

print(Plot.Final)
dev.off()


# Now for females

# Plot for blog: 
# how has the 0.01 mortality hurdle changed for males over time in England & Wales?
require(lattice)
require(latticeExtra)

jpeg("femaleBlog.jpg", height=2000, width=2000)
Part1 <- contourplot(Female ~ Year * Age, 
                     data=this.ds,
                     region=T, 
                     lwd=2,
                     col.regions=rev(heat.colors(200)),
                     cuts=50, 
                     xlab=list(
                       label="Year",
                       cex=2.5),
                     ylab=list(
                       label="Age",
                       cex=2.5
                     ),
                     scales=list(
                       cex=2
                     ),
                     colorkey=F,
                     main="", 
                     labels=F,
                     col="grey",
                     sep="")

Part2 <- contourplot(Female ~ Year * Age,
                     data=this.ds,
                     region=F,
                     labels=F,
                     lwd=3,
                     at=c(0, 0.01, 1),
                     panel=function(...){
                       panel.contourplot(...)
                       panel.segments(x0=c(1840, 1900, 2000, 1840), 
                                      x1=c(1900, 1900, 2000, 2000), 
                                      y0=c(40.4, 0, 0, 63.9), 
                                      y1=c(40.4, 40.4, 63.9, 63.9), 
                                      lty="dashed")
                     }                    
)


Plot.Final <- Part1+ Part2 

print(Plot.Final)
dev.off()


# Want a simple plot of mortality for males aged between 0 and five years 

# Total

rm(list=ls())

current.wd <- "X:/1918 Cohort/1918/main/Data"

setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

this.ds <- DeathRates[["GBRTENW"]]
this.ds <- subset(this.ds, subset=Age<6)

first.year <- min(this.ds$Year)
last.year <- max(this.ds$Year) - 5

df <- data.frame(year=first.year:last.year, fiveyearmort=NA)

for (i in first.year:last.year){

  tmp <- rep(NA, 6)
  for (j in 0:5){
    tmp[j+1] <- this.ds$Total[this.ds$Age==j & this.ds$Year==(i+j)]
  }
  tmp2 <- 1 - tmp
  tmp3 <- 1 - cumprod(tmp2)
  
  df$fiveyearmort[df$year==i] <- tmp3[6]
}

png("BathTubLeft.png", height=800, width=800)
par(mar=c(5,5,5,5))
plot(fiveyearmort ~ year , data=df, type="l", lwd=2, bty="n", axes=F, 
     ylab="Five Year Child Mortality\n", xlab="Year of birth")
polygon(c(1841, 1841, df$year, 2004, 1841), c(0, df$fiveyearmort[1], df$fiveyearmort, 0, 0), , lwd=2, col="grey")
abline(v=c(1850, 1900, 1950, 2000), lty="dashed")
abline(h=c(0.02, 0.05, 0.10, 0.20, 0.25), lty="dashed")
axis(1, at=c(1850, 1900, 1950, 2000), label=c(1850, 1900, 1950, 2000))
axis(3, at=c(1850, 1900, 1950, 2000), label=c(1850, 1900, 1950, 2000))
axis(2, at=c(0.02, 0.05, 0.10, 0.20, 0.25), label=c("1-in-50", "1-in-20", "1-in-10", "1-in-5", "1-in-4"), las=1)
axis(4, at=c(0.02, 0.05, 0.10, 0.20, 0.25), label=c("1-in-50", "1-in-20", "1-in-10", "1-in-5", "1-in-4"), las=1)
dev.off()






## Getting heatmaps (for importing to Cryengine)
rm(list=ls())

current.wd <- "X:/1918 Cohort/1918/main/Data"

setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")


X <- GetValMat(DeathRates, "GBRTENW", "male")
bmp("Heat.bmp", height=1024, width=1024, units="px", bg="black")
image(X, axes=F, useRaster=T, col=gray(1:500/2000))
dev.off()


# EPA Graphics

# Prep work
current.wd <- "X:/1918 Cohort/1918/main/Data"
setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

# unlogged surface plot 
Make3dPlot(DeathRates,country="ITA", sex="male")

# Logged surface plot
Make3dPlot(DeathRates,country="ITA", sex="male", log.it=T)


# Unlogged contour plot
this.ds <- DeathRates[["ITA"]]
this.ds <-   subset(this.ds, Age < 81) 
require(lattice)
tiff(file="../../../EPA/Unlogged.tiff", height=1000, width=800)
contourplot(Male ~ Age * rev(Year), 
            data=this.ds, region=T, 
            col.regions=rev(grey(100:200/200)),
            ylab="Year",
            cuts=50, main="Unlogged") -> X



# Some nasty manual manipulation
print(X)
update(X, 
       scales=list(
         y=list(
           labels=rev(seq(from=1880, to=2020, by=20))
         )
       )
)
dev.off()

tiff(file="../../../EPA/Logged.tiff", height=1000, width=800)

# Logged contour plot
contourplot(log(Male) ~ Age * rev(Year), 
            data=this.ds, region=T, 
            col.regions=rev(grey(100:200/200)),
            ylab="Year",
            cuts=50, main="Logged") -> X



# Some nasty manual manipulation
print(X)
update(X, 
       scales=list(
         y=list(
           labels=rev(seq(from=1880, to=2020, by=20))
         )
       )
)

dev.off()


# Plotting of figure to illustrate difference between plotting a cohort age-mort curve and a cross-sectional age-mort curve

rm(list=ls())

setwd("X:/Manuscripts/1918 Cohort/1918/main/Data/")
source("Functions.R")
load("DerivedData.rData")
Dta <- DeathRates[["GBRTENW"]]
tmp <- GetCohortDeathRates(Dta, cohortYear=1928)
tmp2 <- subset(Dta, subset=Year==2008)
tmp3 <- data.frame(age=tmp2$Age, rate=tmp2$Male)

tiff(file="../../../../SSTE Manuscript/Figures/EngBath.tiff", height=500, width=500)
plot(rate ~ age, log="y", data=tmp, ylim=c(0.0001, 0.1), type="l", col="red", xlab="Age (Years)", ylab="Mortality rate", lwd=2, las=1, yaxp=c(0.0001, 0.1, 1), yaxs="r", yaxt="n")
lines(rate ~ age, data=tmp3, lwd=2, lty="dashed", col="blue")

axis(2, at=c(0.0001, 0.001, 0.01, 0.1), labels=c("0.0001", "0.001", "0.01", "0.1"), las=1)
legend("bottomright", lwd=c(2,2), lty=c("solid", "dashed"), col=c("red" ,"blue"), legend=c("1929 Cohort", " 2008 Cross-sectional"))
dev.off()

# Plotting of figure to illustrate a synthetic cohort:

# CONTOUR PLOTS # - red heat maps
#############################################################################################
rm(list=ls())
current.wd <- "X:/Manuscripts/1918 Cohort/1918/main/Data" 
setwd(current.wd)
source("Functions.r")


load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)
require(latticeExtra)


  this.ds <- DeathRates[["GBRTENW"]]
  this.ds <- subset(this.ds, Age < 81 & Year > 1919) 
  
  
tiff("../../../../SSTE Manuscript/Figures/CohortProjection.tiff",  height=1000, width=1000)

  contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, 
              col.regions=rev(grey(101:200/200)), cuts=20, main="",
              xlim=c(1920, 2090),
              panel=function(...){
                panel.contourplot(...)
                panel.segments(x0=1928, x1=2008, y0=0, y1=80, lwd=2, col="red", lty="solid")
                panel.segments(x0=2008, x1=2008, y0=0, y1=80, lwd=2, col="blue", lty="dashed")
                panel.segments(x0=2008, x1=2088, y0=0, y1=80, lwd=1, col="purple", lty="dashed")

              }
            )


dev.off()


######################################################################################################
## 5/2/2014
## Work on creating STL file for 3d printing
######################################################################################################
# EPA Graphics

# Prep work
rm(list=ls())
current.wd <- "X:/REFERENCE/1918 Cohort/1918/main/Data/"
setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

# Logged surface plot
Make3dPlot(DeathRates,country="ITA", sex="male", log.it=T, return.valmat=T)

# Want to return the output of the rgl persp3d function called in Make3dPlot
# col="lightgrey" 
# specular="black" 
# axes=F 
# box=F 
# xlab="" 
# ylab="" 
# zlab="" 
# 
# tmp2 <- persp3d(tmp,
#         col=col,
#         specular=specular, axes=axes, 
#         box=box, xlab=xlab, ylab=ylab, zlab=zlab)    

require("r2stl")

z <- tmp
x <- 1:nrow(tmp)
y <- 1:ncol(tmp)

z <- z - min(z)

z <- z / max(z)
x <- x - min(x) ; x <- x / max(x)
y <- y - min(y) ; y <- y / max(y)



r2stl(
  x=x,
  y=y,
  z=z,
  
  filename="Italy_Males_Logged_Mort.stl",
  z.expand=T,
  show.persp=T
  )

######################################################################################################
## 5/3/2014
## Population graphics?
######################################################################################################
# EPA Graphics

# Prep work
rm(list=ls())
current.wd <- "X:/REFERENCE/1918 Cohort/1918/main/Data/"
setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")
load("ComingAndGoing.rData")


require(lattice)



for (i in 1:dim(Country.Codes)[1]){
  this.ds <- Populations.numeric[[Country.Codes[i,1]]]
  this.ds <- subset(this.ds, Age < 100) 

  this.ds$Year <- as.numeric(this.ds$Year)
  
  tiff(paste("populations/males/10/Pop_Male_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("populations/males/20/Pop_Male_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
  tiff(paste("populations/males/50/Pop_Male_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
  dev.off()
  
#   tiff(paste("populations/males/100/Pop_Male_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
#   print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Males", sep="")))                   
#   dev.off()

  tiff(paste("populations/females/10/Pop_Female_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()

  tiff(paste("populations/females/20/Pop_Female_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
  tiff(paste("populations/females/50/Pop_Female_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
  dev.off()
  
#   tiff(paste("populations/females/100/Pop_Female_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
#   print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Females", sep="")))                   
#   dev.off()

  tiff(paste("populations/total/10/Pop_Total_10_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=10, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()


  tiff(paste("populations/total/20/Pop_Total_20_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
  tiff(paste("populations/total/50/Pop_Total_50_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
  print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
  dev.off()
  
#   tiff(paste("populations/total/100/Pop_Total_100_", Country.Codes[i,1], ".tif", sep=""),  height=1000, width=1000)
#   print(contourplot(Total ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=100, main=paste(Country.Codes[i,3], ", Total", sep="")))                   
#   dev.off()

}


###################################################################################################
# 26 April 2014
###################################################################################################

# Exploratory analysis to see if there are increasing residuals (real - expected) numbers of 
# young people in more recent years



rm(list=ls())
#current.wd <- "X:/REFERENCE/1918 Cohort/1918/main/Data/"
current.wd <- "/Users/JonMinton/Google Drive/REFERENCE/1918 Cohort/1918/main/Data/"
setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")
load("ComingAndGoing.rData")

# Let's look at people aged under 80 years

#  Start with one country and then generalise

N.countries <- dim(Country.Codes)[1]







residuals.male.list <- vector("list", length=dim(Country.Codes)[1])
residuals.female.list <- vector("list", length=dim(Country.Codes)[1])
residuals.total.list <- vector("list", length=dim(Country.Codes)[1])

expected.male.list <- vector("list", length=dim(Country.Codes)[1])
expected.female.list <- vector("list", length=dim(Country.Codes)[1])
expected.total.list <- vector("list", length=dim(Country.Codes)[1])

for (cntry in 1:N.countries){
    pops.ds <- Populations.numeric[[Country.Codes[cntry,1]]]
    pops.ds <- subset(pops.ds, Age <= 80) 
    pops.ds$Year <- as.numeric(pops.ds$Year)
    
    deaths.ds <- Deaths.numeric[[Country.Codes[cntry,1]]]
    deaths.ds <- subset(deaths.ds, Age <= 80)
    deaths.ds$Year <- as.numeric(deaths.ds$Year)
    
    years <- intersect(unique(pops.ds$Year), unique(deaths.ds$Year))
    ages <- intersect(unique(pops.ds$Age), unique(deaths.ds$Age))

    expected.ds.male <- matrix(NA,
                          nrow=length(ages) - 1,
                          ncol=length(years) - 1
    )
    
    dimnames(expected.ds.male) <- list(
        ages[-1],
        years[-1]
    )
    
    expected.ds.total <- expected.ds.female <- expected.ds.male
    residual.ds.total <- residual.ds.female <- residual.ds.male <- expected.ds.total
    
    for (i in 2:length(ages)){
        for (j in 2:length(years)){
            last.age <- ages[i-1]
            last.year <- years[j-1]
            this.age <- ages[i]
            this.year <- years[j]
            
            tmp1 <- subset(pops.ds, Age==last.age & Year == last.year)
            if (dim(tmp1)[1]!=1) break
            
            lives.expected.male  <- tmp1$Male
            lives.expected.female <- tmp1$Female
            lives.expected.total <- tmp1$Total
            
            tmp2 <- subset(deaths.ds, Age==last.age & Year==last.year)
            if (dim(tmp2)[1]!=1) break
            
            deaths.reported.male <- tmp2$Male
            deaths.reported.female <- tmp2$Female
            deaths.reported.total <- tmp2$Total
            
            lives.expected.male <- lives.expected.male - deaths.reported.male
            lives.expected.female <- lives.expected.female - deaths.reported.female
            lives.expected.total <- lives.expected.total - deaths.reported.total
            
            if (length(lives.expected.male)==1){   expected.ds.male[ i - 1, j - 1] <- lives.expected.male }
            if (length(lives.expected.female)==1) { expected.ds.female[i - 1, j - 1] <- lives.expected.female}
            if (length(lives.expected.total)==1) { expected.ds.total[i - 1, j -1 ] <- lives.expected.total}            
            
            tmp3 <- subset(pops.ds, Age==this.age & Year==this.year)
            if (dim(tmp3)[1]!=1) break
            
            lives.actual.male <- tmp3$Male
            lives.actual.female <- tmp3$Female
            lives.actual.total <- tmp3$Total
            
            lives.residual.male <- lives.expected.male - lives.actual.male
            lives.residual.female <- lives.expected.female - lives.actual.female
            lives.residual.total <- lives.expected.total - lives.actual.total
            
            
            
            if (length(lives.residual.male)==1) { residual.ds.male[   i - 1, j - 1] <- lives.residual.male }
            if (length(lives.residual.female)==1) { residual.ds.female[i - 1, j - 1] <- lives.residual.female}
            if (length(lives.residual.total)==1) {residual.ds.total[i - 1, j - 1] <- lives.residual.total}
        }
    }
 
    
    residuals.male.list[[cntry]] <- residual.ds.male
    residuals.female.list[[cntry]] <- residual.ds.female
    residuals.total.list[[cntry]] <- residual.ds.total
    
    expected.male.list[[cntry]] <- expected.ds.male
    expected.female.list[[cntry]] <- expected.ds.female
    expected.total.list[[cntry]] <- expected.ds.total
    
}


# Now to save this all to .csv files

dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/")

dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/")
dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/")

dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Male/")
dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Female/")
dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Total/")

dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Male/")
dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Female/")
dir.create("/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Total/")





for (i in 1:N.countries){
    
    write.csv(
        residuals.male.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Male/",
            Country.Codes[i,1], "_Residuals_Males.csv"
            )
    )
    
    write.csv(
        residuals.female.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Female/",
            Country.Codes[i,1], "_Residuals_Females.csv"
        )
    )
    
    write.csv(
        residuals.total.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Residuals/Total/",
            Country.Codes[i,1], "_Residuals_Total.csv"
        )
    )

    
    write.csv(
        expected.male.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Male/",
            Country.Codes[i,1], "_Expectations_Males.csv"
        )
    )
    
    write.csv(
        expected.female.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Female/",
            Country.Codes[i,1], "_Expectations_Females.csv"
        )
    )
    
    write.csv(
        expected.total.list[[i]],
        file=paste0(
            "/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Expectations/Total/",
            Country.Codes[i,1], "_Expectations_Total.csv"
        )
    )
    
}


write.csv(Country.Codes,
          file="/Users/JonMinton/Google Drive/PROJECTS/Age Residuals/Country Codes.csv"
          )

max(abs(residuals.male.list[[25]])) -> limt
levelplot(residuals.male.list[[25]], at=c(-limt, -lmt/2, 0, lmt/2, lmt))
levelplot(residuals.male.list[[25]], at=c(-limt, -lmt/2, 0, limt/2, limt))
levelplot(residuals.male.list[[25]], at=c(-limt, -limt/2, 0, limt/2, limt))
levelplot(residuals.male.list[[25]], at=c(-limt, -limt/2, 0, limt/2, limt), scales=list(draw=F))
image(residuals.male.list[[25]])
?image
levelplot(residuals.female.list[[25]], at=c(-limt, -limt/2, 0, limt/2, limt), scales=list(draw=F))
levelplot(residuals.male.list[[25]], at=c(-limt, -limt/2, 0, limt/2, limt), scales=list(draw=F))
levelplot(residuals.male.list[[25]], at=c(-limt, -limt*2/3, -limt/3, 0, limt/3, limt*2/3, limt), scales=list(draw=F))
levelplot(residuals.male.list[[15]], at=c(-limt, -limt*2/3, -limt/3, 0, limt/3, limt*2/3, limt), scales=list(draw=F))
levelplot(residuals.male.list[[30]], at=c(-limt, -limt*2/3, -limt/3, 0, limt/3, limt*2/3, limt), scales=list(draw=F))
for (i in 1:48){levelplot(residuals.male.list[[i]], at=c(-limt, -limt*2/3, -limt/3, 0, limt/3, limt*2/3, limt), scales=list(draw=F)); browser()}
for (i in 1:48){print(levelplot(residuals.male.list[[i]], at=c(-limt, -limt*2/3, -limt/3, 0, limt/3, limt*2/3, limt), scales=list(draw=F))); browser()}
i
i
i
i
i
i
i
i
i
i

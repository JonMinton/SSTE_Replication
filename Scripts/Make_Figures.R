
# Jon Minton
# 27/5/2014
# Code for making figures in Minton 2014



# Fig 2: Heatmap representation of mortality surface, for males (total), England & Wales, 1847-2009

dir.create("Figures", showWarnings=F)
tiff("Figures/Figure01.tiff", height=1000, width=1000)
gs.50 <- rev(grey(seq(0,1, by=0.02)))
this.ds <- DeathRates[["GBRTENW"]] # England & Wales, Total
this.ds <- subset(this.ds, Age < 91) 
print(    
    contourplot(
        Male ~ Year * Age, 
        data=this.ds, 
        contour=F, 
        region=T, 
        col.regions=gs.50,
        at=quantile(this.ds$Male, probs=seq(0.02, 1, by=0.02), na.rm=T), 
        main="England & Wales (Total), Males"
    )                   
)
dev.off()



# Fig 4: five year child mortality, males, England & Wales

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

tiff("Figures/Figure04.tiff", height=800, width=800)
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




# Fig 5: Crude mortality rates as a function of age, males, 1929 cohort; 2008 cross sectional

Dta <- DeathRates[["GBRTENW"]]
tmp <- GetCohortDeathRates(Dta, cohortYear=1928)
tmp2 <- subset(Dta, subset=Year==2008)
tmp3 <- data.frame(age=tmp2$Age, rate=tmp2$Male)

tiff(file="Figures/Figure05.tiff", height=500, width=500)
plot(rate ~ age, log="y", data=tmp, ylim=c(0.0001, 0.1), type="l", col="red", xlab="Age (Years)", ylab="Mortality rate", lwd=2, las=1, yaxp=c(0.0001, 0.1, 1), yaxs="r", yaxt="n")
lines(rate ~ age, data=tmp3, lwd=2, lty="dashed", col="blue")

axis(2, at=c(0.0001, 0.001, 0.01, 0.1), labels=c("0.0001", "0.001", "0.01", "0.1"), las=1)
legend("bottomright", lwd=c(2,2), lty=c("solid", "dashed"), col=c("red" ,"blue"), legend=c("1929 Cohort", " 2008 Cross-sectional"))
dev.off()


# Fig 6: shaded contour plot of log mortality rates for males, England & Wales, 1920 to 2009, with added lines


this.ds <- DeathRates[["GBRTENW"]]
this.ds <- subset(this.ds, Age < 81 & Year > 1919) 


tiff("Figures/Figure06.tiff",  height=1000, width=1000)
print(   
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
)
dev.off()


# Fig 7: Shaded contour plot, Norway, males


this.ds <- DeathRates[["NOR"]]
this.ds <- subset(this.ds, Age < 81) 

tiff("Figures/Figure07.tiff", height=1000, width=1000)

print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, 
                  main="Norway, Males")
)
dev.off()


# Fig 8: Shaded contour plot. Norway, females

tiff("Figures/Figure08.tiff", height=1000, width=1000)

print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50, 
                  main="Norway, Females")
)
dev.off()


# Fig 9: Shaded log mortality surface, Ukraine, Males

this.ds <- DeathRates[["UKR"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure09.tiff", height=1000, width=1000)

print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Ukraine, Males"
)
)
dev.off()


# Fig 10: Shaded log mortality surface, Ukraine, Females

tiff("Figures/Figure10.tiff", height=1000, width=1000)

print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Ukraine, Females"
))

dev.off()


# Fig 11: Shaded log mortality surface, USA, Males

this.ds <- DeathRates[["USA"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure11.tiff", height=1000, width=1000)

print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="United States, Males"
)
)
dev.off()


# Fig 12: Shaded log mortality surface, USA, Females

tiff("Figures/Figure12.tiff", height=1000, width=1000)

print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="United States, Females"
)
)
dev.off()


# Fig 13: Difference in log mortality surfaces, USA, Log(males) - log (females)

tiff("Figures/Figure13.tiff", height=1000, width=1000)
print(contourplot(
    I(log(Male) - log(Female)) ~ Year * Age, 
    data=this.ds, region=T,
    col.regions=rev(heat.colors(200)),
    cuts=20,
    main="United States, Difference"
)
)
dev.off()

# Fig 14: Males, Finland

this.ds <- DeathRates[["FIN"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure14.tiff", height=1000, width=1000)

print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Finland, Males"
)
)
dev.off()



# Fig 15: Males, Sweden

this.ds <- DeathRates[["SWE"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure15.tiff", height=1000, width=1000)

print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Sweden, Males"
)
)
dev.off()



# Fig 16: Males, Russia

this.ds <- DeathRates[["RUS"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure16.tiff", height=1000, width=1000)

print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Russia, Males"
)
)
dev.off()


# Fig 17: Log Males, Japan

this.ds <- DeathRates[["JPN"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Figure17.tiff", height=1000, width=1000)

print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Japan, Males"
)
)
dev.off()


# Fig 18: Log Females, Japan

tiff("Figures/Figure18.tiff", height=1000, width=1000)

print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=50,
                  main="Japan, Females"
)
)
dev.off()




#################
this.ds <- DeathRates[["GBR_SCO"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Male_LogDeath.tiff", height=1200, width=1200)

print(contourplot(log(Male) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20,
                  main="Males, Log of death rates"
)
)
dev.off()

this.ds <- DeathRates[["GBR_SCO"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Female_LogDeath.tiff", height=1200, width=1200)

print(contourplot(log(Female) ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20,
                  main="Females, Log of death rates"
)
)
dev.off()

this.ds <- Populations.numeric[["GBR_SCO"]]
this.ds <- subset(this.ds, Age < 81)

tiff("Figures/Male_Pop.tiff", height=1200, width=1200)

print(contourplot(Male ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20,
                  main="Males, Population counts"
)
)
dev.off()


tiff("Figures/Female_Pop.tiff", height=1200, width=1200)

print(contourplot(Female ~ Year * Age, data=this.ds, region=T, col.regions=rev(heat.colors(200)), cuts=20,
                  main="Females, Population counts"
)
)
dev.off()


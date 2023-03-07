library(tidyverse)
#read data
GHG <- read.csv("data input/data of ghg emission.csv",header=T)
GHG[260,] <- NA
GHG[260,c(3,20:23)] <- c(1994,0,0,0,0) #To fill the year gap
#First plot is to know the trend of GHG emission
#Are we turning our focus from CO2 to CH4 and N2O?
ghg.year <- data.frame(year = c(1993:2022),CO2 = rep(0,30)
                     ,CH4 = rep(0,30),N2O = rep(0,30)
                     ,NH3 = rep(0,30))  #empty table
for (i in 1:4) {
ghg.year[i + 1] <- tapply(GHG[,19 + i],GHG$Pub..year,sum)
}

cols <- c("#9EC9E2","#3C93C2","#0D4A70"
        ,"#22BB3B")

#plot
year_pdf <- "data output/Publication in year.pdf"
pdf(file = year_pdf)
plot(ghg.year$year,ghg.year$CO2,type = "n"
     ,ylim = c(0,18),ylab = "Total Publication"
     ,xlab = "Publish year",las = 1)
for (i in 1:4) {
lines(ghg.year$year,ghg.year[,i + 1],col = cols[i],
      lwd = 2,lty = i)
}

legend(1996,16,colnames(ghg.year)[2:5]
       ,col = cols,lty = c(1:4),bty = "n",lwd = 2)
dev.off()

#We want to spatial distribtion of the studies
cols.bar <- terrain.colors(12)
prov <- (unique(GHG$Region))[c(8,2,10,1,5,4,11,7,12,9,6,3)]
prov.n <- length(prov)
#GHG empty datagframe to store data by region.
ghg.prov <- data.frame(prov = prov,
                       CO2 = rep(0, prov.n),
                       CH4 = rep(0, prov.n),
                       N2O = rep(0, prov.n),
                       NH3 = rep(0, prov.n))
for (i in 1:4) {
  ghg.prov[i+1] <- tapply(GHG[, 19+i], GHG$Region, sum)[prov]
}
#Save barplot of GHG emissions by region to PDF
prov_pdf <- "data output/Publication in province.pdf"
pdf(file = prov_pdf)

barplot(as.matrix(ghg.prov[, 2:5]),
        beside = TRUE,
        ylim = c(0, 60),
        col = cols.bar,
        las = 1,
        ylab = "Total Publication")

legend(1, 60, prov, bty = "n",
       ncol = 3, fill = cols.bar)

dev.off()

#Research type
rtype <- c("Lab", "Pilot", "Farm", "Field", "LCA")
rtype.n <- length(rtype)

rtype.prov <- data.frame(prov = prov,
                         Lab = rep(0, prov.n),
                         Pilot = rep(0, prov.n),
                         Farm = rep(0, prov.n),
                         Field = rep(0, prov.n),
                         LCA = rep(0, prov.n))

#obtain research type in the regions
for (i in 1:prov.n) {
  for (j in 1:rtype.n) {
    rtype.prov[i, j+1] <- sum(str_detect(GHG$Region, prov[i]) &
                                str_detect(GHG$Research.type, rtype[j]))
  }
}
#Save bar plot of researcg types by region to pdf
Rtype_pdf <- "data output/Research types in province.pdf"
pdf(file = Rtype_pdf)

barplot(as.matrix(rtype.prov[, 2:6]),
        beside = TRUE,
        ylim = c(0, 30),
        col = cols.bar,
        las = 1,
        ylab = "Number of Research type")

legend(1, 30, prov, bty = "n",
       ncol = 3, fill = cols.bar)

dev.off()

#Count the journal numbers
jour<-unique(GHG$Journal[!is.na(GHG$Journal)])
jour.n<-length(jour)
ghg.jour<-data.frame(Journal=jour,num=rep(0,jour.n))  #empty table
for (i in 1:jour.n) {
  ghg.jour[i,2]<-sum(str_detect(GHG$Journal[!is.na(GHG$Journal)],jour[i]))
}


#Count the use of techniques
tech<-unique(GHG$Technique)[!is.na(GHG$Technique)])
tech.n<-length(tech)
ghg.tech<-data.frame(prov=tech,tech=rep(0,tech.n))  #empty table
for (i in 1:length(tech)) {
ghg.tech[i,2]<-sum(str_detect(GHG$Technique[!is.na(GHG$Technique)],tech[i]))
}

#Trends of techniques used in 1993-2022
tech<-unique(GHG$Technique[!is.na(GHG$Technique)])[c(1:4,6,8,10)]
year.tech<-data.frame(matrix(c(1993:2022,1:210),nrow=30,ncol=8)) # empty dataframe
names(year.tech)<-c("year",tech)

library(tidyverse)
#read data
GHG<-read.csv("data input/data of ghg emission.csv",header=T)
GHG[263,]<-NA
GHG[263,c(3,20:23)]<-c(1994,0,0,0,0) #To fill the year gap
#First plot is to know the trend of GHG emission
#Are we turning our focus from CO2 to CH4 and N2O?
ghg.year<-data.frame(year=c(1993:2022),CO2=rep(0,30)
                     ,CH4=rep(0,30),N2O=rep(0,30)
                     ,NH3=rep(0,30))  #empty table
for(i in 1:4) {
ghg.year[i+1]<-tapply(GHG[,19+i],GHG$Pub..year,sum)
}

cols<-c("#9EC9E2","#3C93C2","#0D4A70"
        ,"#22BB3B")

#plot
year_pdf<-"data output/Publication in year.pdf"
pdf(file = year_pdf)
plot(ghg.year$year,ghg.year$CO2,type="n"
     ,ylim=c(0,18),ylab="Total Publication"
     ,xlab="Publish year",las=1)
for(i in 1:4){
lines(ghg.year$year,ghg.year[,i+1],col=cols[i],
      lwd=2,lty=i)
}

legend(1996,16,colnames(ghg.year)[2:5]
       ,col=cols,lty=c(1:4),bty="n",lwd=2)
dev.off()

#We want to spatial distribtion of the studies
prov<-(unique(GHG$Region))[c(8,2,10,1,5,4,11,7,12,9,6,3)]
prov.n<-length(prov)
ghg.prov<-data.frame(prov=prov,CO2=rep(0,prov.n)
                     ,CH4=rep(0,prov.n),N2O=rep(0,prov.n)
                     ,NH3=rep(0,prov.n))  #empty table
for(i in 1:4) {
  ghg.prov[i+1]<-tapply(GHG[,19+i],GHG$Region,sum)
}
cols.bar<-terrain.colors(12)
prov_pdf<-"data output/Publication in province.pdf"
pdf(file=prov_pdf)
barplot(as.matrix(ghg.prov[,2:5])
        ,beside=TRUE,ylim=c(0,60)
        ,col=cols.bar,las=1
        ,ylab="Total Publication")

legend(1,60,ghg.prov[,1],bty="n",
       ncol=3,fill =cols.bar)
dev.off()


#Count the use of techniques
tech<-unique(GHG$Technique[!is.na(GHG$Technique)])
tech.n<-length(tech)
ghg.tech<-data.frame(prov=tech,tech=rep(0,tech.n))  #empty table
for (i in 1:length(tech)) {
ghg.tech[i,2]<-sum(str_detect(GHG$Technique[!is.na(GHG$Technique)],tech[i]))
}

#Trends of techniques used in 1993-2022
tech<-unique(GHG$Technique[!is.na(GHG$Technique)])[c(1:4,6,8,10)]
year.tech<-data.frame(matrix(c(1993:2022,1:210),nrow=30,ncol=8))
names(year.tech)<-c("year",tech)

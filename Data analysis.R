#read data
GHG<-read.csv("data input/data of ghg emission.csv",header=T)

#First plot is to know the trend of GHG emission
#Are we turning our focus from CO2 to CH4 and N2O?
ghg.year<-data.frame(year=c(1996:2022),CO2=rep(0,27)
                     ,CH4=rep(0,27),N2O=rep(0,27)
                     ,NH3=rep(0,27))  #empty table
for(i in 1:4) {
ghg.year[i+1]<-tapply(GHG[,19+i],GHG$Pub..year,sum)
}

cols<-c("#9EC9E2","#3C93C2","#0D4A70"
        ,"#22BB3B")
#plot
pdf(file = "Publication in year.pdf")
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
prov<-sort(unique(GHG$Region))
ghg.prov<-data.frame(prov=prov,CO2=rep(0,11)
                     ,CH4=rep(0,11),N2O=rep(0,11)
                     ,NH3=rep(0,11))  #empty table
for(i in 1:4) {
  ghg.prov[i+1]<-tapply(GHG[,19+i],GHG$Region,sum)
}
cols.bar<-terrain.colors(11)
pdf(file="Publication in province.pdf")
barplot(as.matrix(ghg.prov[,2:5])
        ,beside=TRUE,ylim=c(0,50)
        ,col=cols.bar,las=1
        ,ylab="Total Publication")

legend(1,50,ghg.prov[,1],bty="n",
       ncol=3,fill =cols.bar)
dev.off()


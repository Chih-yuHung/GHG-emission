#This is to plot four figures that show the 
#1. Research type + tech, 2. livestock 
#3. Manure collect +treatments. 4. GHG
#by provinces 
#Bubble plot to present data 
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scatterpie)
#read data
GHG<-read.csv("data input/data of ghg emission.csv",header=T)

#We want to spatial distribution of the studies
cols.bar <- terrain.colors(12)
prov <- (unique(GHG$Region))[c(8,2,10,1,5,4,11,7,12,9,6,3)]
prov.n <- length(prov)

#1.1 Research type
rtype<-c("Lab","Pilot","Farm","Field","LCA")
rtype.n<-length(rtype)
rtype.prov<-data.frame(prov=prov,Lab=rep(0,prov.n)
                       ,Pilot=rep(0,prov.n),Farm=rep(0,prov.n)
                       ,Field=rep(0,prov.n),LCA=rep(0,prov.n))  #empty table
#obtain research type in the regions
for (i in 1:prov.n) {
  for (j in 1:rtype.n) {
    rtype.prov[i,j+1]<-sum(str_detect(GHG$Region[!is.na(GHG$Region)],prov[i]) &
                             str_detect(GHG$Research.type[!is.na(GHG$Research.type)],rtype[j])
    ) 
  }
}

#turn wide to long
rtype.prov <- rtype.prov %>% 
  pivot_longer(rtype,names_to = "type",values_to = "number")
#Replace regions' names because it's too long to draw
rtype.prov$prov<-rep(c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","Rgnl.","Ntl."),each=5)
#Keep the order for my plot
rtype.prov$prov <- factor(rtype.prov$prov,levels=unique(rtype.prov$prov))
rtype.prov$type <- factor(rtype.prov$type,levels=unique(rtype.prov$type))

rtype.prov$locate<-as.numeric(c(1:12))#rep(1,length(rtype.prov$prov))
rtype.prov$xloc<-as.numeric(c(1:12))
rtype.prov$prov<-as.numeric(c(1:12))
ggplot() + geom_scatterpie(aes(x=xloc, y=locate, group=prov)
                           ,data=rtype.prov)
                           #cols=LETTERS[1:5]) #
                            #+ coord_equal()

#Output Research types in provinces
Rtype_pdf<-"data output/Research types in province_Bubble.pdf"
pdf(file=Rtype_pdf)
ggplot(rtype.prov, aes(x = prov, y = type)) + 
  geom_point(aes(size = number,fill = str_to_title(type)),
             alpha = 0.75, shape = 21) +
  scale_size_continuous(limits = c(0.000001, 30), range = c(1,15), breaks = c(1,5,10,20)) +
  labs( x= "Region", y = "Research Type", size = "Research counts", fill = "") +  
  scale_fill_discrete(guide="none") +
  # geom_text(aes(label = number), 
  #           colour = "black",
  #           size = 3) 
  theme(legend.key=element_blank(),
  axis.text.x = element_text(colour = "black", size = 11, angle = 90, vjust = 0.3, hjust = 1),
  axis.text.y = element_text(colour = "black", size = 11),
  legend.text = element_text(size = 12, colour ="black"),
  legend.title = element_text(size = 12),
  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
  legend.position = "top")
  # scale_fill_manual(guide = FALSE)  
  #scale_y_discrete(limits = rev(levels(rtype.prov$number))) 
dev.off()


#1.2 Research tech
rtech <- c("Incubation","Soil chamber","Collar","Animal chamber"
         ,"Micrometeorology","Modelling","Mixed")
rtech.n <- length(rtech)
rtech.prov <- data.frame(prov = prov
                       ,Incubation = rep(0,prov.n)
                       ,"Soil chamber" = rep(0,prov.n)
                       ,Collar = rep(0,prov.n)
                       ,"Animal chamber" = rep(0,prov.n)
                       ,Micrometeorology = rep(0,prov.n)
                       ,Modelling = rep(0,prov.n)
                       ,Mixed = rep(0,prov.n),check.names = F)  #empty table
#obtain research type in the regions
for (i in 1:prov.n) {
  for (j in 1:rtech.n) {
    rtech.prov[i,j+1]<-sum(str_detect(GHG$Region[!is.na(GHG$Region)],prov[i]) &
                             str_detect(GHG$Technique[!is.na(GHG$Technique)],rtech[j])
    ) 
  }
}
#turn wide to long
rtech.prov <- rtech.prov %>% 
  pivot_longer(rtech,names_to = "tech",values_to = "number")
#Replace regions' names because it's too long to draw
rtech.prov$prov<-rep(c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","Rgnl.","Ntl."),each=7)
#Keep the order for my plot
rtech.prov$prov <- factor(rtech.prov$prov,levels=unique(rtech.prov$prov))
rtech.prov$type <- factor(rtech.prov$tech,levels=unique(rtech.prov$tech))
#Plot output
Rtech_pdf<-"data output/Research techs in province_Bubble.pdf"
pdf(file=Rtech_pdf)
ggplot(rtech.prov, aes(x = prov, y = tech)) + 
  geom_point(aes(size = number,fill = str_to_title(tech)),
             alpha = 0.75, shape = 21) +
  scale_size_continuous(limits = c(0.000001, 30), range = c(1,15), breaks = c(1,5,10,20)) +
  labs( x= "Region", y = "Research Techniques", size = "Research counts", fill = "") +  
  scale_fill_discrete(guide="none") +
  # geom_text(aes(label = number), 
  #           colour = "black",
  #           size = 3) 
  theme(legend.key=element_blank(),
        axis.text.x = element_text(colour = "black", size = 11, angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 11),
        legend.text = element_text(size = 12, colour ="black"),
        legend.title = element_text(size = 12),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.position = "top")
# scale_fill_manual(guide = FALSE)  
#scale_y_discrete(limits = rev(levels(rtype.prov$number))) 
dev.off()






#4. GHG
ghg.prov<-data.frame(prov=prov,CO2=rep(0,prov.n)
                     ,CH4=rep(0,prov.n),N2O=rep(0,prov.n)
                     ,NH3=rep(0,prov.n))  #empty table
for(i in 1:4) {
  ghg.prov[i+1]<-tapply(GHG[,19+i],GHG$Region,sum)
}

prov_pdf<-"data output/Publication in province.pdf"
pdf(file=prov_pdf)
barplot(as.matrix(ghg.prov[,2:5])
        ,beside=TRUE,ylim=c(0,60)
        ,col=cols.bar,las=1
        ,ylab="Total Publication")

legend(1,60,ghg.prov[,1],bty="n",
       ncol=3,fill =cols.bar)
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

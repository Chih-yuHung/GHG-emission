#This script is to analyze the data for GHG emissions from manure-applied field
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scatterpie)
#read data
GHG <- read.csv("data input/data of ghg emission.csv",header=T)
#obtain studies with field measurement 
GHG.field <- GHG[grep("Field", GHG$GHG.source), ]

#1. Spatial analysis
#We want to spatial distribution of the studies
cols.bar <- terrain.colors(12)
prov <- unique(GHG.field$Region)[c(7,2,9,6,3,5,10,11,12,8,4,1)]
prov.n <- length(prov)

#number of studies in AB, ON, and QC, 96 studies, 68%
GHG.AB <- GHG.field[grep(prov[2], GHG.field$Region), ]
GHG.ON <- GHG.field[grep(prov[5], GHG.field$Region), ]
GHG.QC <- GHG.field[grep(prov[6], GHG.field$Region), ]


#2. Temporal analysis
# Year-Round vs. other, 53 studies, 53/141 =38%
GHG.year <- GHG.field[grep(("Year-Round"),GHG.field$Season),]



#3. Livestock types
GHG.Dairy <- GHG.field[grep(("Dairy"),GHG.field$Livestock),] #69
GHG.Beef <- GHG.field[grep(("Beef"),GHG.field$Livestock),] #37
GHG.Swine <- GHG.field[grep(("Swine"),GHG.field$Livestock),] # 44
GHG.Poultry <- GHG.field[grep(("Poultry"),GHG.field$Livestock),] #12


#4. Manure types
GHG.Solid <- GHG.field[grep(("Solid"),GHG.field$Manure.type),] #87
GHG.Liquid <- GHG.field[grep(("Liquid"),GHG.field$Manure.type),] #94
GHG.both <- GHG.field[grep(("Liquid, Solid"),GHG.field$Manure.type),] #42



#GHG types
sum(GHG.field$CO2) #64
sum(GHG.field$CH4) #41
sum(GHG.field$N2O) #122
sum(GHG.field$NH3) #16

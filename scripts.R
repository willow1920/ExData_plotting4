## set directory
#setwd("GoogleDrive/workshop/Exploratory Data Analysis with R/week4")
dir()
list.files()

## remove variables
#rm(list=ls())
ls()

## read file
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

## Question
## 1. Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? Using the base plotting system, make a plot showing 
## the total PM2.5 emission from all sources for each of the years 
## 1999, 2002, 2005, and 2008.

sumyear<-tapply(NEI$Emissions,NEI$year,sum)
barplot(sumyear,xlab="year",ylab="Total PM2.5 Emissions",main="Total Emissions in the US")
dev.copy(png,"plot1.png")
dev.off()

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland ( fips == "24510") from 1999 to 2008? Use the base plotting 
## system to make a plot answering this question.

baltimore<-subset(NEI,fips=="24510")
sumyearsub1<-tapply(baltimore$Emissions,baltimore$year,sum)
barplot(sumyearsub1,xlab="year",ylab="Total PM2.5 Emissions",main="Total emissions from PM2.5 in the Baltimore City Maryland")
dev.copy(png,"plot2.png")
dev.off()

## 3. Of the four types of sources indicated by the type 
## (point, nonpoint, onroad, nonroad) variable, which of these four 
## sources have seen decreases in emissions from 1999–2008 for 
## Baltimore City? Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)
plot.new()
sumyeartype<-with(baltimore,tapply(Emissions,list(year,type),sum))
sumyeartype<-as.data.frame(as.table(sumyeartype))
names(sumyeartype)<-c("year","type","TotalEmission")
qplot(year,TotalEmission,data=sumyeartype,color=type)
ggsave("plot3.png")
#dev.copy(png,"plot3.png")
dev.off()

## 4. Across the United States, how have emissions from coal 
## combustion-related sources changed from 1999–2008?

coalSCC<-SCC[grep("Coal",SCC$EI.Sector),"SCC"]
coalNEI<-subset(NEI, SCC %in% coalSCC)
sumyearcoal<-tapply(coalNEI$Emissions,coalNEI$year,sum)
barplot(sumyearcoal,xlab="year",ylab="Total Emissions",main="Emissions from coal combustion-related sources")
dev.copy(png,"plot4.png")
dev.off()

## 5. How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?

motorSCC<-SCC[grep("Motor",SCC$SCC.Level.Three),"SCC"]
motorNEI<-subset(NEI, SCC %in% motorSCC)
motorNEIbt<-subset(motorNEI,fips=="24510")
sumyearmotorbt<-tapply(motorNEIbt$Emissions,motorNEIbt$year,sum)
barplot(sumyearmotorbt,xlab="year",ylab="Total Emissions",main="Emissions from motor vehiecle sources in Baltimore City")
dev.copy(png,"plot5.png")
dev.off()

## 6. Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, 
## California (fips == "06037"). Which city has seen greater changes 
## over time in motor vehicle emissions?

motorNEIcf<-subset(motorNEI,fips=="06037")
sumyearmotorcf<-tapply(motorNEIcf$Emissions,motorNEIcf$year,sum)
sumyearmotorbt<-as.data.frame(as.table(sumyearmotorbt))
sumyearmotorcf<-as.data.frame(as.table(sumyearmotorcf))
colnames(sumyearmotorbt)<-c("year","Baltimore")
colnames(sumyearmotorcf)<-c("year","LosAngeles")
sumyearmotor2<-merge(sumyearmotorbt,sumyearmotorcf,by.x = "year")
## otherwise will save the previous plot
plot.new()
#png(file="plot6.png")
ggplot(data=sumyearmotor2,aes(x=year))+geom_point(aes(y=Baltimore,color="Baltimore"))+geom_point(aes(y=LosAngeles,color="LosAngeles"))+ylab("Total Emissions")+title("Emissions from motor vehicles for Baltimore and Los Angeles")+scale_colour_manual("",values = c("Baltimore"="blue","LosAngeles"="red"))
ggsave("plot6.png")
#dev.copy(png,"plot6.png")
dev.off()


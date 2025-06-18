rm(list = ls())
###################
## Load Packages ##
###################
library(plyr)
library(lubridate)
library(reshape)
library(data.table)
library(lattice)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(segmented)
library(hrbrthemes)
library(viridis)
library(ggridges)

## import required datasets
df.wk.summary <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklySummary_by_Species_18June2025.rds")
setwd("~/github/Timing-Windows/03_Output/")

wk.prop.sum<-ddply(subset(df.wk.summary,Rank!="Low"), c("Week","Species"), summarise, 
                   Mean.Wk.Prop = mean(Prop.Year,na.rm=T),
                   SD.Wk.Prop = sd(Prop.Year,na.rm=T),
                   Max.Wk.Prop = max(Prop.Year,na.rm=T),
                   Min.Wk.Prop = min(Prop.Year,na.rm=T)) #
wk.prop.by.species = cast(wk.prop.sum, Week~Species,value="Mean.Wk.Prop") 
wk.prop.by.species[is.na(wk.prop.by.species)] = 0 
head(wk.prop.by.species)
head(df.wk.summary)







df.high<-subset(df.wk.summary,Rank=="High")
df.mod<-subset(df.wk.summary,Rank=="Mod")
df.low<-subset(df.wk.summary,Rank=="Low")

p <- ggplot(data=subset(df.high,SpawnTemp=="Warm"),aes(x=Week,y=Prop.Year,color=Year))
p <- p +  labs(y= "Week", x = "Prop. Annual Total Catch")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_smooth(colour = "black")
p <- p + facet_wrap(~ Species)
p 

p <- ggplot(data=subset(df.high,SpawnTemp=="Cold"),aes(x=Week,y=Prop.Year,color=Year))
p <- p +  labs(y= "Week", x = "Prop. Annual Total Catch")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_smooth(colour = "black")
p <- p + facet_wrap(~ Species)
p 

p <- ggplot(data=df.mod,aes(x=Week,y=Prop.Year,color=Year))
p <- p +  labs(y= "Week", x = "Prop. Annual Total Catch")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_smooth(colour = "black")
p <- p + facet_wrap(~ Species)
p 






p <- ggplot(data=df.high,aes(x=Week,y=Prop.Year,color=Year))
p <- p +  labs(y= "Week", x = "Prop. Annual Total Catch)")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
#p <- p + xlim(0,5)
#p <- p + ylim(0,200)
#p <- p + geom_line(data = my.model, aes(x = logCatch, y = Window), colour = "tomato",lwd=2)
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_smooth(colour = "black")
p <- p + facet_wrap(~ Species)
p 







p <- ggplot(data=df.c,aes(x=logCatch,y=Window,color=Species))
p <- p +  labs(y= "Capture Window (days)", x = "log10(Annual Total Catch)")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p + xlim(0,5)
p <- p + ylim(0,200)
p <- p + geom_line(data = my.model, aes(x = logCatch, y = Window), colour = "tomato",lwd=2)
p 



p <- ggplot(data=df.g,aes(x=logCatch,y=Window,color=Species))
p <- p +  labs(y= "Capture Window (weeks)", x = "log10(Annual Total Catch)")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p + xlim(0,5)
#p <- p + ylim(0,25)
#p <- p + geom_line(data = my.model, aes(x = logCatch, y = Window), colour = "tomato",lwd=2)
p 





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
#library(glatos)

df.plot <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklyMeans_by_RAP_18June2025.rds")
setwd("~/github/Timing-Windows/03_Output/")

p <- ggplot(data=subset(df.plot,Rank!="Low"),aes(Week, fSpecies2 , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + scale_y_discrete(limits=rev)
#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

png("Fishway_MeanWeeklyProp_by_Species_CommonFishes_OrderByMaxWeek_18June2025.png",
    width = 2600, height = 1600,units="px",res=300)
p
dev.off()


p <- ggplot(data=subset(df.plot,Rank=="High"),aes(Week, fSpecies2 , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + scale_y_discrete(limits=rev)
#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

png("Fishway_MeanWeeklyProp_by_Species_VeryCommonFishes_OrderByMaxWeek_18June2025.png",
    width = 2600, height = 1600,units="px",res=300)
p
dev.off()

p <- ggplot(data=subset(df.plot,Rank=="Low"),aes(Week, fSpecies , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + scale_y_discrete(limits=rev)
#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

png("Fishway_MeanWeeklyProp_by_Species_UnCommonFishes_18June2025.png",
    width = 2600, height = 1600,units="px",res=300)
p
dev.off()
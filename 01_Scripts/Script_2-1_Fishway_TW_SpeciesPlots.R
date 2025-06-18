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
df.wk.plot <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklyMeans_by_Species_18June2025.rds")
setwd("~/github/Timing-Windows/03_Output/")

## can swamp Prop.Year for Week.Sum to show the actual catch values
##Bigmouth Buffalo
p <- ggplot(data=subset(df.wk.plot,Species=="Bigmouth Buffalo"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Bigmouth Buffalo") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_BigmouthBuffalo_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Black Bullhead
p <- ggplot(data=subset(df.wk.plot,Species=="Black Bullhead"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Black Bullhead") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_BlackBullhead_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Bowfin
p <- ggplot(data=subset(df.wk.plot,Species=="Bowfin"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Bowfin") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_Bowfin_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Brown Bullhead
p <- ggplot(data=subset(df.wk.plot,Species=="Brown Bullhead"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_BrownBullhead_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Channel Catfish
p <- ggplot(data=subset(df.wk.plot,Species=="Channel Catfish"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Channel Catfish") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_ChannelCatfish_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Common Carp
p <- ggplot(data=subset(df.wk.plot,Species=="Common Carp"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Common Carp") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_CommonCarp_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Freshwater Drum
p <- ggplot(data=subset(df.wk.plot,Species=="Freshwater Drum"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Freshwater Drum") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_FreshwaterDrum_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Gizzard Shad
p <- ggplot(data=subset(df.wk.plot,Species=="Gizzard Shad"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Gizzard Shad") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_GizzardShad_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Goldfish
p <- ggplot(data=subset(df.wk.plot,Species=="Goldfish"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Goldfish") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_Goldfish_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Largemouth Bass
p <- ggplot(data=subset(df.wk.plot,Species=="Largemouth Bass"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Largemouth Bass") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_Largemouth Bass_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Northern Pike
p <- ggplot(data=subset(df.wk.plot,Species=="Northern Pike"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Northern Pike") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_NorthernPike_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Rainbow Trout
p <- ggplot(data=subset(df.wk.plot,Species=="Rainbow Trout"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Rainbow Trout") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_RainbowTrout_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## RRudd
p <- ggplot(data=subset(df.wk.plot,Species=="Rudd"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Rudd") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_Rudd_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Bass
p <- ggplot(data=subset(df.wk.plot,Species=="White Bass"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Bass") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_WhiteBass_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Perch
p <- ggplot(data=subset(df.wk.plot,Species=="White Perch"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Perch") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_WhitePerch_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Sucker
p <- ggplot(data=subset(df.wk.plot,Species=="White Sucker"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Sucker") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_WhiteSucker_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Yellow Perch
p <- ggplot(data=subset(df.wk.plot,Species=="Yellow Perch"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Yellow Perch") 
p <- p + scale_y_discrete(limits=rev)
p

png("Fishway_YellowPerch_by_Year_18June2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()


#############################################
## Species-specific based on overall catch ##
#############################################
## Shows the same thing as just hte raw catch fro each species...
df.wk.overall.plot<-merge(df.wk.plot,sum.species,by=c("Species"),all=T) ## combine yearly
df.wk.overall.plot<-plyr::rename(df.wk.overall.plot,c("Quantity"="Overall.Sum"))
df.wk.overall.plot[is.na(df.wk.overall.plot)] = 0 

df.wk.overall.plot$Prop.Overall<-df.wk.overall.plot$Week.Sum/df.wk.overall.plot$Overall.Sum

## Common Carp
p <- ggplot(data=subset(df.wk.overall.plot,Species=="Common Carp"),aes(Week, fYear , fill=Prop.Overall))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Common Carp") 
p

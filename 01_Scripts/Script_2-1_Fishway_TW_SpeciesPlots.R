# create species-specific plots 
rm(list = ls())
###################
## Load Packages ##
###################
library(plyr)
library(lubridate)
library(reshape)
library(data.table)
library(lattice)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
df.wk.plot <- readRDS("02_Data/TW_Fishway_WeeklyMeans_by_Species_03July2025.rds") 
setwd("~/github/Timing-Windows/03_Output/")

# FIGURE 2
## can swamp Prop.Year for Week.Sum to show the actual catch values
## Bowfin
p <- ggplot(data=subset(df.wk.plot,Species=="Bowfin"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Bowfin") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_Bowfin_by_Year_03July2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Channel Catfish
p <- ggplot(data=subset(df.wk.plot,Species=="Channel Catfish"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Channel Catfish") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_ChannelCatfish_by_Year_03July2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Northern Pike
p <- ggplot(data=subset(df.wk.plot,Species=="Northern Pike"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Northern Pike") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_NorthernPike_by_Year_03July2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Sucker
p <- ggplot(data=subset(df.wk.plot,Species=="White Sucker"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Sucker") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_WhiteSucker_by_Year_03July2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()


### Supplemental Figure S2
## Brown Bullhead
p <- ggplot(data=subset(df.wk.plot,Species=="Bowfin"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_BrownBullhead_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Common Carp
p <- ggplot(data=subset(df.wk.plot,Species=="Common Carp"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Common Carp") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_CommonCarp_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Freshwater Drum
p <- ggplot(data=subset(df.wk.plot,Species=="Freshwater Drum"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Freshwater Drum") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_FreshwaterDrum_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Gizzard Shad
p <- ggplot(data=subset(df.wk.plot,Species=="Gizzard Shad"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Gizzard Shad") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_GizzardShad_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Goldfish
p <- ggplot(data=subset(df.wk.plot,Species=="Goldfish"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Goldfish") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_Goldfish_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Largemouth Bass
p <- ggplot(data=subset(df.wk.plot,Species=="Largemouth Bass"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Largemouth Bass") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_Largemouth Bass_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Rainbow Trout
p <- ggplot(data=subset(df.wk.plot,Species=="Rainbow Trout"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Rainbow Trout") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_RainbowTrout_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Rudd
p <- ggplot(data=subset(df.wk.plot,Species=="Rudd"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Rudd") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_Rudd_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Bass
p <- ggplot(data=subset(df.wk.plot,Species=="White Bass"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Bass") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_WhiteBass_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## White Perch
p <- ggplot(data=subset(df.wk.plot,Species=="White Perch"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - White Perch") 
p <- p + scale_y_discrete(limits=rev)
#p <- p + scale_fill_viridis(option = "B", values = scales::rescale(c(.5, 0.5, 1)))
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p

png("Fishway_WhitePerch_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

## Yellow Perch
p <- ggplot(data=subset(df.wk.plot,Species=="Yellow Perch"),aes(Week, fYear , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p +  labs(y= "Year", x = "Week", fill="Weekly 
Prop. Total")
p <- p +  ggtitle("Cootes Paradise Fishway - Yellow Perch") 
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "#D55E00")
p
p.YP

png("Fishway_YellowPerch_by_Year_17Sept2025.png",
    width = 2800, height = 2400,units="px",res=300)
p
dev.off()

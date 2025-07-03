rm(list = ls())
###################
## Load Packages ##
###################
library(plyr)
library(lubridate)
library(reshape)
library(data.table)
library(lattice)
#library(rgdal) depreciated
#library(rgeos)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
#library(glatos)

#df.plot <- readRDS("02_Data/TW_Fishway_WeeklyMeans_by_RAP_18June2025.rds") #PB
df.plot <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklyMeans_by_RAP_03July2025.rds") ##JM

#setwd("~/github/Timing-Windows/03_Output/")

p <- ggplot(data=subset(df.plot,Rank!="Low"),aes(Week, fSpecies2 , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys")) # Greens / Reds /Greys
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=1)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=1)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="blue")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="red") 

#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

png("Fishway_MeanWeeklyProp_by_Species_CommonFishes_OrderByMaxWeek_03July2025.png",
    width = 2600, height = 1600,units="px",res=300)
p
dev.off()


p <- ggplot(data=subset(df.plot,Rank=="High"),aes(Week, fSpecies2 , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys")) #Greens / Reds
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=1)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=1)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="blue")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="red") 

p <- p + scale_y_discrete(limits=rev)
#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

png("Fishway_MeanWeeklyProp_by_Species_VeryCommonFishes_OrderByMaxWeek_03July2025.png",
    width = 2600, height = 1600,units="px",res=300)
p
dev.off()


## this plot no longer works b/c most of these species have <10 individuals/year
p <- ggplot(data=subset(df.plot,Rank=="Low"),aes(Week, fSpecies , fill=Prop.Year))
p <- p +  geom_tile(color="grey50")
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Greys"))
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=1)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=1)
p <- p +  labs(y= "Species", x = "Week", fill="Weekly 
Prop. Total")
p <- p + scale_y_discrete(limits=rev)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="blue")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha=0.1, fill="red") 

#p <- p +  ggtitle("Cootes Paradise Fishway - Brown Bullhead") 
p

#png("Fishway_MeanWeeklyProp_by_Species_UnCommonFishes_03July2025.png",
#     width = 2600, height = 1600,units="px",res=300)
# p
#dev.off()
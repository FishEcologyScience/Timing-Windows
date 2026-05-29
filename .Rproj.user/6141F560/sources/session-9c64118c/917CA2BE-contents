## model and plot logistic relationships
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
library(segmented)
library(viridis)
library(ggridges)

#################
## Import Data ##
#################
## import base data to assign thermal guilds and define order
df.plot <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklyMeans_by_RAP_16Years_29May2026.rds") 
df.plot<-subset(df.plot,Rank!="Low")
df.plot.base<-aggregate(Quantity~Species+fSpecies2+SpawnTemp,data=df.plot,FUN=mean)

## import base data for yearly proportion by EP
df.cool <- readRDS("~/github/Timing-Windows/02_Data/Fishway_SumProportion_in_CoolRAPPeriod_by_Species_Year_29May2026.rds")
df.cool <- df.cool[, c("Year", "Species","Rank","CoolRAPStatus","SumProp")]
df.cool$RAP <- as.factor("Coolwater")
df.cool$Period <- paste(df.cool$RAP,sep="-",df.cool$CoolRAPStatus)
df.cool$CoolRAPStatus=NULL
df.warm <- readRDS("~/github/Timing-Windows/02_Data/Fishway_SumProportion_in_WarmRAPPeriod_by_Species_Year_29May2026.rds")
df.warm <- df.warm[, c("Year", "Species","Rank","WarmRAPStatus","SumProp")]
df.warm$RAP <- as.factor("Warmwater")
df.warm$Period <- paste(df.warm$RAP,sep="-",df.warm$WarmRAPStatus)
df.warm$WarmRAPStatus=NULL

setwd("~/github/Timing-Windows/03_Output/")

## combine cool warm EP data
combo<-rbind(df.cool,df.warm)

## remove the "protected" proportion
combo <- combo[combo$Period != "Coolwater-RAP", ]
combo <- combo[combo$Period != "Warmwater-RAP", ]

## summarize by EP (adds the pre/post period for each)
combo.by.EP <-aggregate(SumProp~Year+Species+RAP,data=combo,FUN=sum)
interim <- subset(combo,Period=="Coolwater-PreRAP"|Period=="Warmwater-PostRAP") ## pull out start ends
ul <-unique(df.cool$Year) ## make a list with Year with coolwater data
interim.ul<-interim[interim$Year %in% ul, ] ## isolate from interim the Years with data for both EP
combo.both.EP <-aggregate(SumProp~Year+Species,data=interim.ul,FUN=sum) ## combine data from both available EPs
combo.both.EP$RAP <- as.factor("Combined")
combo.both.EP <- combo.both.EP[, c("Year", "Species","RAP","SumProp")]
combo.plotting <- rbind(combo.both.EP,combo.by.EP)
combo.plotting$fRAP <- factor(combo.plotting$RAP, levels = c("Coolwater", "Warmwater", "Combined"))

for.plotting<-merge(combo.plotting,df.plot.base,by="Species")

levels(for.plotting$fSpecies2)[levels(for.plotting$fSpecies2) == "Bowfin"] <- "Emerald Bowfin"

## FIGURE 1
p <- ggplot(data=for.plotting,aes(SumProp, fSpecies2, shape = fRAP, color = fRAP))
p <- p + annotate("rect", xmin = 0.0, xmax = 0.1, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray45")
p <- p + annotate("rect", xmin = 0.1, xmax = 0.3, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray70")
p <- p +  geom_point(size=2,position = position_jitter(width = 0.0, height = 0.2))
p <- p +  scale_shape_manual(values = c(25, 22, 16))   
p <- p +  scale_color_manual(values = c("blue3","red3","gray40")) 
p <- p +  facet_grid(rows = vars(SpawnTemp), scales = "free_y", space = "free_y")  # separate panels per SpawnTemp
p <- p +  theme_bw(base_size = 20) 
p <- p +  theme(panel.grid=element_blank(),legend.title = element_text(size=10),legend.text = element_text(size=10))
p <- p +  geom_vline(xintercept=0.1, linetype="dashed",colour="gray20", size=1)
p <- p +  geom_vline(xintercept=0.3, linetype="dotdash",colour="gray60", size=1)
p <- p +  scale_y_discrete(limits=rev)
p <- p +  labs(y= "Species", x = "Proportion of Capture Outside Exclusion Period(s)", 
shape="     Exclusion
       Period",
color="     Exclusion
       Period")
p

png("Fishway_RiskOfExposure_AllYear_29May2026.png",
    width = 3000, height = 2800,units="px",res=300)
p
dev.off()

## assigning efficacy
combo.plotting$Efficacy<-ifelse(combo.plotting$SumProp<=0.1,"Highly Effective", 
                                ifelse(combo.plotting$SumProp>0.1&combo.plotting$SumPro<=0.3,"Effective",
                                       "Not Effective"))

table(combo.plotting$Species,combo.plotting$Efficacy,combo.plotting$RAP)


efficacy<-ddply(combo.plotting, c("RAP","Species"), summarise, 
                  Mean.Effectiveness = mean(SumProp,na.rm=T),
                  SD.Effectiveness = sd(SumProp,na.rm=T),
                  Records = length(Species)) 
## save for Table 2

### save for table
test<-ddply(combo, c("RAP","Period","Species"), summarise, 
                MeanProp = mean(SumProp,na.rm=T),
                SDProp = sd(SumProp,na.rm=T),
                Records = length(Species)) 
test2<-ddply(combo, c("RAP","Species"), summarise, 
            MeanProp = mean(SumProp,na.rm=T),
            SDProp = sd(SumProp,na.rm=T),
            Records = length(Species)) 



rm(list = ls())
# determine overlap between species observations (or habitat processes) and timing windows
# https://www.dfo-mpo.gc.ca/pnw-ppe/timing-periodes/on-eng.html
# RAPS - southern region of Ontario
# Spring Spawning
# Walleye: March 15 - May 31 (DOY 74 - 151) # Week 11-22
# Pike: March 15 - May 31 (DOY 74 - 151) # Week 11-22
# Large/Smallmouth Bass: May 01 to July 15 (DOY 121 - 196) # Week 18-29
# Other/Unk Spawning: March 15 to July 15 (DOY 74 - 196) # Week 11-29
#
# Fall Spawning
# Lake Trout: Oct 1 to May 31 (DOY 274 - 151)
# Lake Whitefish/Lake Herring: Oct 15 to May 31 (DOY 288 - 151)
# Other/Unk Spawning: Oct 1 to May 31 (DOY 274 - 151)
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

#################
## Import Data ##
#################
setwd("~/github/Timing-Windows/02_Data/")

# base Fishway dataset - includes data up to 2022
df <- read.csv("~/github/Timing-Windows/02_Data/RBG_Barrier Data_1996-2022.csv")
df$YMD <- dmy(df$Date) # make this into a date object
df$Year <- year(df$YMD) # make this into a date object
df$Day <- yday(df$YMD) # make this into a date object
df$Week <- week(df$YMD)
df<-subset(df,Day>=59) # 28 February - drops two records
df<-subset(df,Day<=243) # 31 August 107154-103940

#Base dataset
df.Years<-unique(df$Year)
df.RAPStatus<-as.factor(c("RAP","PreRAP","PostRAP"))
df.Years.rep<-rep(df.Years,length(df.RAPStatus))
df.RAPStatus.rep<-rep(df.RAPStatus,length(df.Years))
df.Years.rep<-sort(df.Years.rep)
df.base<-data.frame(df.Years.rep,df.RAPStatus.rep)
df.base<-plyr::rename(df.base,c("df.Years.rep"="Year","df.RAPStatus.rep"="RAPStatus"))

df.base.no96<-subset(df.base,Year>1996) ## drops 1996 since limited sampling that year.
df.base.no96<-droplevels(df.base.no96)

### Plot of Lift Days by Year with RAP windows
df.days<-as.data.frame(unique(df$Date))
df.days$Date <- dmy(df.days$'unique(df$Date)') # make this into a date object
df.days$Year <- year(df.days$Date) # make this into a date object
df.days$Day <- yday(df.days$Date) # make this into a date object
df.days$fYear <- as.factor(df.days$Year)
df.days$holder <- 1

p <- ggplot(df.days, aes(Day, fYear)) 
p <- p + theme_bw(base_size = 20) 
p <- p + theme(axis.title = element_text(face = "bold"),legend.position="none")  
p <- p + geom_point(aes(size = holder)) #, shape=15, size=4, position=pd,color="black"
p <- p + labs(y= "Year", x = "Day of Year") 
p <- p + ggtitle("Cootes Paradise Fishway - Lift Days") 
p <- p + geom_vline(xintercept=74, linetype="dashed",colour="blue", linewidth=2)
p <- p + geom_vline(xintercept=151, linetype="dashed",colour="blue", linewidth=2)
p <- p + geom_vline(xintercept=121, linetype="dotdash",colour="red", linewidth=2)
p <- p + geom_vline(xintercept=196, linetype="dotdash",colour="red", linewidth=2)
p <- p + scale_y_discrete(limits=rev)
#p <- p + geom_vline(xintercept=243, linetype="solid",colour="yellow", size=2)
#p <- p + geom_vline(xintercept=50, linetype="solid",colour="yellow", size=2)
p

## SAVE and EXPORT
#png("Fishway_LiftDays_By_Year_23May2025.png", 
#  width = 2400, height = 2400,units="px",res=300)
#p
#dev.off()

# summarize effor by year for each RAP
effort.sum<-ddply(df, c("Year"), summarise, 
                  LastLiftDay = max(Day,na.rm=T),
                  FirstLiftDay = min(Day,na.rm=T),
                  NumberLiftDays = length(unique(Date))) 
effort.sum$Pre_CoolWater_RAPAvailable<-ifelse(effort.sum$FirstLiftDay<=74,"Yes","No")
effort.sum$Pre_CoolWater_RAPDuration<-ifelse(effort.sum$Pre_CoolWater_RAPAvailable=="Yes",effort.sum$FirstLiftDay-74,NA)
effort.sum$Pre_Warmwater_RAPAvailable<-ifelse(effort.sum$FirstLiftDay<=121,"Yes","No")
effort.sum$Pre_Warmwater_RAPDuration<-ifelse(effort.sum$Pre_Warmwater_RAPAvailable=="Yes",effort.sum$FirstLiftDay-121,NA)
#effort.sum$CoolWater_RAPAvailable<-ifelse(effort.sum$FirstLiftDay<=151&effort.sum$FirstLiftDay<=74,"Yes","No")
#effort.sum$CoolWater_RAPDuration<-ifelse(effort.sum$CoolWater_RAPAvailable=="No",effort.sum$FirstLiftDay-152,NA)

## SAVE and EXPORT
#write.csv(effort.sum,file="Fishway_LiftEffort_By_Year_23May2025.csv") 


## combine effort with base data


df.2<-merge(df,effort.sum,by=c("Year"),all=T)
df.3<-df.2[!(df.2$Year==1996|df.2$Year==1999),] ## more breaks in data record for these years, so should be dropped. 
df.3$fYear<-as.factor(df.3$Year)

saveRDS(df.3, file = "TW_Fishway_BaseDataset_18June2025.rds")


# total annual catch by species Table 
year.sum.species<-aggregate(Quantity~Year+Species,data=df.3,FUN=sum)
sum.by.species = cast(year.sum.species, Year~Species,value="Quantity") 
sum.by.species[is.na(sum.by.species)] = 0 
head(sum.by.species)
sum.species<-aggregate(Quantity~Species,data=year.sum.species,FUN=sum)
sum.species$Rank<-ifelse(sum.species$Quantity<=50,"Low",
                    ifelse(sum.species$Quantity>50&sum.species$Quantity<=1000,"Mod","High"))
sum.species$SpawnTemp<-ifelse(sum.species$Species=="Yellow Perch"|sum.species$Species=="Rainbow Trout"|
                                sum.species$Species=="Northern Pike"|sum.species$Species=="Gizzard Shad"|sum.species$Species=="White Sucker","Cold","Warm")

## SAVE and EXPORT
#write.csv(sum.species,file="Fishway_TotalCatch_by_Species_23May2025.csv") 
#write.csv(sum.by.species,file="Fishway_TotalCatch_by_Year_by_Species_23May2025.csv") 
saveRDS(sum.species, file = "TW_Fishway_SumBySpecies_18June2025.rds")

## Weekly summaries
# Weekly dataset
df.Week.Years<-aggregate(Quantity~Year+Week,data=df.3,FUN=sum)
df.species<-c(unique(df.3$Species))
df.Week.Years.rep<-df.Week.Years[rep(seq_len(nrow(df.Week.Years)), length(df.species)), ]
df.species.rep<-rep(df.species,nrow(df.Week.Years))
df.Week.Years.rep <-df.Week.Years.rep[order(df.Week.Years.rep$Year, df.Week.Years.rep$Week),]
df.week.base<-data.frame(df.Week.Years.rep,df.species.rep)
df.week.base<-plyr::rename(df.week.base,c("df.species.rep"="Species"))
df.week.base$Quantity=NULL ## create empty dataset with every week when sampling occurred

df.wk<-aggregate(Quantity~Species+Year+Week,data=df.3,FUN=sum) ## summarize catch by week-year
df.year<-aggregate(Quantity~Species+Year,data=df.3,FUN=sum) ## summarize catch by year

df.wk.temp<-merge(df.week.base,df.wk,by=c("Year","Week","Species"),all=T) ## combine weekly catch
df.wk.temp[is.na(df.wk.temp)] = 0 

df.wk.plot<-merge(df.wk.temp,df.year,by=c("Species","Year"),all=T) ## combine yearly
df.wk.plot<-plyr::rename(df.wk.plot,c("Quantity.x"="Week.Sum","Quantity.y"="Year.Sum"))
df.wk.plot[is.na(df.wk.plot)] = 0 
df.wk.plot$fYear<-as.factor(df.wk.plot$Year)
df.wk.plot$Prop.Year<-df.wk.plot$Week.Sum/df.wk.plot$Year.Sum
df.wk.plot$fSpecies<-as.factor(df.wk.plot$Species)
df.wk.plot[is.na(df.wk.plot)] = 0 ## check if necessary (converts prop. years from NA to zero)

## split out cool/warm fishes
df.wk.plot.split<-merge(df.wk.plot,sum.species,by="Species",all=T)
df.wk.plot.split.cool<-subset(df.wk.plot.split,SpawnTemp=="Cold")
df.wk.plot.split.warm<-subset(df.wk.plot.split,SpawnTemp=="Warm")

# weekly means 
# dropping years with little to no sampling prior to the start of the coolwater RAP
df.wk.plot.mean.warm<-aggregate(Prop.Year~Species+Week,data=df.wk.plot.split.warm,FUN=mean)

df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool,fYear!="1997")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2003")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2004")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2005")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2007")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2008")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2014")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2015")
df.wk.plot.split.cool.sub<-subset(df.wk.plot.split.cool.sub,fYear!="2022")

df.wk.plot.2<-rbind(df.wk.plot.split.warm,df.wk.plot.split.cool.sub) # combine back base warm and modified cool datasets

df.wk.plot.mean.cool<-aggregate(Prop.Year~Species+Week,data=df.wk.plot.split.cool.sub,FUN=mean)

df.wk.plot.mean<-rbind(df.wk.plot.mean.warm,df.wk.plot.mean.cool)
df.wk.plot.mean$fSpecies<-as.factor(df.wk.plot.mean$Species)
df.wk.plot.mean.2<-merge(df.wk.plot.mean,sum.species,by="Species",all=T)

df.wk.plot.mean.2$fSpecies2 <- factor(df.wk.plot.mean.2$fSpecies, levels = c("Rainbow Trout", "Northern Pike", "White Sucker", "Yellow Perch", "Rudd", "Brown Bullhead",
                                          "Black Crappie","Bowfin",'White Perch',"Goldfish","Largemouth Bass","Black Bullhead",
                                          "Bigmouth Buffalo","White Bass","Common Carp","Common Carp x Goldfish","Channel Catfish",
                                          "Freshwater Drum","Gizzard Shad"))

write.csv(df.wk.plot.mean.2,file="TW_Fishway_WeeklyMeans_by_RAP_18June2025.csv")
saveRDS(df.wk.plot.mean.2, file = "TW_Fishway_WeeklyMeans_by_RAP_18June2025.rds")

write.csv(df.wk.plot,file="TW_Fishway_WeeklyMeans_by_Species_18June2025.csv")
saveRDS(df.wk.plot, file = "TW_Fishway_WeeklyMeans_by_Species_18June2025.rds")



## mean weekly proportion of the population passing (risk-based look-up table?)
df.wk.summary<-df.wk.plot.2
wk.prop.sum<-ddply(subset(df.wk.summary,Rank!="Low"), c("Week","Species"), summarise, 
                    Mean.Wk.Prop = mean(Prop.Year,na.rm=T),
                    SD.Wk.Prop = sd(Prop.Year,na.rm=T),
                    Max.Wk.Prop = max(Prop.Year,na.rm=T),
                    Min.Wk.Prop = min(Prop.Year,na.rm=T)) #
wk.prop.by.species = cast(wk.prop.sum, Week~Species,value="Mean.Wk.Prop") 
wk.prop.by.species[is.na(wk.prop.by.species)] = 0 
head(wk.prop.by.species)
saveRDS(df.wk.summary, file = "TW_Fishway_WeeklySummary_by_Species_18June2025.rds")


## SAVE and EXPORT
#write.csv(wk.prop.by.species,file="Fishway_MeanCumulativeCatch_by_Species_23May2025.csv")

wk.prop.by.species.min = cast(wk.prop.sum, Week~Species,value="Min.Wk.Prop") 
wk.prop.by.species.min[is.na(wk.prop.by.species.min)] = 0 
head(wk.prop.by.species.min)

## SAVE and EXPORT
#write.csv(wk.prop.by.species.min,file="Fishway_MinCumulativeCatch_by_Species_23May2025.csv")

wk.prop.by.species.max = cast(wk.prop.sum, Week~Species,value="Max.Wk.Prop") 
wk.prop.by.species.max[is.na(wk.prop.by.species.max)] = 0 
head(wk.prop.by.species.max)

## SAVE and EXPORT
#write.csv(wk.prop.by.species.max,file="Fishway_MaxCumulativeCatch_by_Species_23May2025.csv")

##
## Risk? ##
df.wk.summary$CoolRAPStatus<-as.factor(ifelse(df.wk.summary$Week>=11&df.wk.summary$Week<=22,"RAP",
                                          ifelse(df.wk.summary$Week<11,"PreRAP","PostRAP")))
df.wk.summary$WarmRAPStatus<-as.factor(ifelse(df.wk.summary$Week>=18&df.wk.summary$Week<=29,"RAP",
                                              ifelse(df.wk.summary$Week<29,"PreRAP","PostRAP")))
df.wk.summary$FullRAPStatus<-as.factor(ifelse(df.wk.summary$Week>=11&df.wk.summary$Week<=29,"RAP",
                                              ifelse(df.wk.summary$Week<11,"PreRAP","PostRAP")))

df.wk.sum.effort<-merge(df.wk.summary,effort.sum,by=c("Year"),all=T)

df.wk.summary.cool<-ddply(na.omit(subset(df.wk.sum.effort,Pre_CoolWater_RAPAvailable=="Yes"&Rank!="Low")), c("Year","Species","Rank","CoolRAPStatus"), summarise, 
                 SumProp= sum(Prop.Year,na.rm=T),
                 MeanProp= mean(Prop.Year,na.rm=T),
                 SDProp = sd(Prop.Year,na.rm=T),
                 MaxProp = max(Prop.Year,na.rm=T),
                 MinProp = min(Prop.Year,na.rm=T),
                 Records = length(Prop.Year)) ## 

df.cool<-aggregate(SumProp~Species+CoolRAPStatus,data=df.wk.summary.cool,FUN=mean)
df.cool.by.status = cast(df.cool, Species~CoolRAPStatus,value="SumProp") 
df.cool.by.status[is.na(df.cool.by.status)] = 0 
df.cool.by.status
df.cool.sd<-aggregate(SumProp~Species+CoolRAPStatus,data=df.wk.summary.cool,FUN=sd)
df.cool.by.status.sd = cast(df.cool.sd, Species~CoolRAPStatus,value="SumProp") 
df.cool.by.status.sd[is.na(df.cool.by.status.sd)] = 0 
df.cool.by.status.sd
df.cool.max<-aggregate(SumProp~Species+CoolRAPStatus,data=df.wk.summary.cool,FUN=max)
df.cool.by.status.max = cast(df.cool.max, Species~CoolRAPStatus,value="SumProp") 
df.cool.by.status.max[is.na(df.cool.by.status.max)] = 0 
df.cool.by.status.max
df.cool.min<-aggregate(SumProp~Species+CoolRAPStatus,data=df.wk.summary.cool,FUN=min)
df.cool.by.status.min = cast(df.cool.min, Species~CoolRAPStatus,value="SumProp") 
df.cool.by.status.min[is.na(df.cool.by.status.min)] = 0 
df.cool.by.status.min

## SAVE and EXPORT
#write.csv(df.cool.by.status,file="Fishway_MeanProp_in_CoolRAPPeriod_by_Species_23May2025.csv")
#write.csv(df.cool.by.status.sd,file="Fishway_SDProp_in_CoolRAPPeriod_by_Species_22Aug2023.csv")


df.cool.by.status.ranges<-ddply(df.wk.summary.cool, c("Species","CoolRAPStatus"), summarise, 
                          MeanSumProp= mean(SumProp,na.rm=T),
                          SDSumProp = sd(SumProp,na.rm=T),
                          MaxSumProp = max(SumProp,na.rm=T),
                          MinSumProp = min(SumProp,na.rm=T),
                          Records = length(SumProp)) ## 



df.wk.summary.warm<-ddply(na.omit(subset(df.wk.sum.effort,Rank!="Low")), c("Year","Species","Rank","WarmRAPStatus"), summarise, 
                          SumProp= sum(Prop.Year,na.rm=T),
                          MeanProp= mean(Prop.Year,na.rm=T),
                          SDProp = sd(Prop.Year,na.rm=T),
                          MaxProp = max(Prop.Year,na.rm=T),
                          MinProp = min(Prop.Year,na.rm=T)) ## 

df.warm<-aggregate(SumProp~Species+WarmRAPStatus,data=df.wk.summary.warm,FUN=mean)
df.warm.by.status = cast(df.warm, Species~WarmRAPStatus,value="SumProp") 
df.warm.by.status[is.na(df.warm.by.status)] = 0 
df.warm.by.status
df.warm.sd<-aggregate(SumProp~Species+WarmRAPStatus,data=df.wk.summary.warm,FUN=sd)
df.warm.by.status.sd = cast(df.warm.sd, Species~WarmRAPStatus,value="SumProp") 
df.warm.by.status.sd[is.na(df.warm.by.status.sd)] = 0 
df.warm.by.status.sd
df.warm.max<-aggregate(SumProp~Species+WarmRAPStatus,data=df.wk.summary.warm,FUN=max)
df.warm.by.status.max = cast(df.warm.max, Species~WarmRAPStatus,value="SumProp") 
df.warm.by.status.max[is.na(df.warm.by.status.max)] = 0 
df.warm.by.status.max
df.warm.min<-aggregate(SumProp~Species+WarmRAPStatus,data=df.wk.summary.warm,FUN=min)
df.warm.by.status.min = cast(df.warm.min, Species~WarmRAPStatus,value="SumProp") 
df.warm.by.status.min[is.na(df.warm.by.status.min)] = 0 
df.warm.by.status.min
df.warm.by.status.ranges<-ddply(df.wk.summary.warm, c("Species","WarmRAPStatus"), summarise, 
                                MeanSumProp= mean(SumProp,na.rm=T),
                                SDSumProp = sd(SumProp,na.rm=T),
                                MaxSumProp = max(SumProp,na.rm=T),
                                MinSumProp = min(SumProp,na.rm=T),
                                Records = length(SumProp)) ## 

## SAVE and EXPORT
# write.csv(df.warm.by.status,file="Fishway_MeanProp_in_WarmRAPPeriod_by_Species_23May2025.csv")
# write.csv(df.warm.by.status.sd,file="Fishway_SDProp_in_WarmRAPPeriod_by_Species_22Aug2023.csv")

year.sum<-year.sum.species ## number at fishway per year
year.sum<-plyr::rename(year.sum,c("Quantity"="TotalCatch"))

saveRDS(year.sum, file = "TW_Fishway_YearlySum_by_Species_18June2025.rds")

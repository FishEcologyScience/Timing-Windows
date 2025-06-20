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
library(lme4)
library(car)
library(lmerTest)
library(performance)
library(lsmeans)
library(multcomp)
library(ggeffects)

## import required datasets
df.wk.summary <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklySummary_by_Species_18June2025.rds")
setwd("~/github/Timing-Windows/03_Output/")

## remove years with zero catch
df.wk.summary <- df.wk.summary[df.wk.summary$Year.Sum != 0, ] 


## cumulative sum by species x year
df.cumul.sum <- df.wk.summary %>%
       group_by(fSpecies, fYear) %>%
       arrange(fSpecies, fYear,Week) %>%  # optional: make sure data is ordered
       mutate(cumul.sum = cumsum(Prop.Year))

wk.cum.sum<-ddply(subset(df.cumul.sum,Rank!="Low"), c("Week","Species"), summarise, 
                   Mean.Wk.Prop = mean(cumul.sum,na.rm=T),
                   SD.Wk.Prop = sd(cumul.sum,na.rm=T),
                   Max.Wk.Prop = max(cumul.sum,na.rm=T),
                   Min.Wk.Prop = min(cumul.sum,na.rm=T)) #
wk.cum.by.species = cast(wk.cum.sum, Week~Species,value="Mean.Wk.Prop") 
wk.cum.by.species[is.na(wk.cum.by.species)] = 0 
#write.csv(wk.cum.by.species,file="Mean_Weekly_Cumulative_PropCatch_19June2025.csv")
# Table 3 #

## break out by how common species are
df.high<-subset(df.cumul.sum,Rank=="High")
df.mod<-subset(df.cumul.sum,Rank=="Mod")
df.low<-subset(df.cumul.sum,Rank=="Low")

## plot for most common species
p <- ggplot(data=df.high,aes(x=Week,y=cumul.sum,color=Species))
p <- p +  labs(y= "Cumulative Prop. Annual Total Catch", x = "Week")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p + geom_smooth()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=1)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=1)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=1)
p <- p + scale_y_continuous(limits = c(0,1))
#p <- p + facet_wrap(~ Species)
p 


#### mixed model - pike test case
df.pike<-subset(df.high,fSpecies=="Northern Pike")
m1 <- glmer(cumul.sum ~ Week + (1 | fYear), 
               data = df.pike, 
               family = binomial)
summary(m1) ## model does not converge..
new.pike <- expand.grid(Week = unique(df.pike$Week),
                        fYear = unique(df.pike$fYear))

new.pike$predicted_prob <- predict(m1, new.pike, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_jitter(data = df.pike, 
                     aes(x = Week, y = cumul.sum), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.pike, 
                    aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
p <- p + geom_smooth(data=df.pike,aes(x=Week,y=cumul.sum))
p 






## non cumulative plots and summaries
## calcualte mean weekly props by species.
wk.prop.sum<-ddply(subset(df.wk.summary,Rank!="Low"), c("Week","Species"), summarise, 
                   Mean.Wk.Prop = mean(Prop.Year,na.rm=T),
                   SD.Wk.Prop = sd(Prop.Year,na.rm=T),
                   Max.Wk.Prop = max(Prop.Year,na.rm=T),
                   Min.Wk.Prop = min(Prop.Year,na.rm=T)) #
wk.prop.by.species = cast(wk.prop.sum, Week~Species,value="Mean.Wk.Prop") 
wk.prop.by.species[is.na(wk.prop.by.species)] = 0 
head(wk.prop.by.species) 

## plot just for warm water fishes
p <- ggplot(data=subset(df.high,SpawnTemp=="Warm"),aes(x=Week,y=Prop.Year,color=Year))
p <- p +  labs(y= "Week", x = "Prop. Annual Total Catch")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", size=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", size=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", size=2)
p <- p + geom_smooth(colour = "black")
#p <- p + facet_wrap(~ Species)
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

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
library(performance)

## import required datasets
df.wk.summary <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklySummary_by_Species_03July2025.rds")
setwd("~/github/Timing-Windows/03_Output/")

## remove years with zero catch
df.wk.summary <- df.wk.summary[df.wk.summary$Year.Sum != 0, ] 


## cumulative sum by species x year
df.cumul.prop <- df.wk.summary %>%
       group_by(fSpecies, fYear) %>%
       arrange(fSpecies, fYear,Week) %>%  # optional: make sure data is ordered
       mutate(cumul.prop = cumsum(Prop.Year))
df.cumul.catch <- df.wk.summary %>%
  group_by(fSpecies, fYear) %>%
  arrange(fSpecies, fYear,Week) %>%  # optional: make sure data is ordered
  mutate(cumul.catch = cumsum(Week.Sum))
df.cumul.prop$cumul.catch<-df.cumul.catch$cumul.catch

wk.cumul.prop<-ddply(subset(df.cumul.prop,Rank!="Low"), c("Week","Species"), summarise, 
                   Mean.Wk.Prop = mean(cumul.prop,na.rm=T),
                   SD.Wk.Prop = sd(cumul.prop,na.rm=T),
                   Max.Wk.Prop = max(cumul.prop,na.rm=T),
                   Min.Wk.Prop = min(cumul.prop,na.rm=T)) #
wk.cumul.by.species = cast(wk.cumul.prop, Week~Species,value="Mean.Wk.Prop") 
wk.cumul.by.species[is.na(wk.cumul.by.species)] = 0 
#write.csv(wk.cumul.by.species,file="Mean_Weekly_Cumulative_PropCatch_03July025.csv")
# Table 3 #

## break out by how common species are
df.high<-subset(df.cumul.prop,Rank=="High")
df.mod<-subset(df.cumul.prop,Rank=="Mod")
df.low<-subset(df.cumul.prop,Rank=="Low")

## plot for most common species
p <- ggplot(data=df.high,aes(x=Week,y=cumul.prop,color=Species))
p <- p +  labs(y= "Cumulative Prop. Annual Total Catch", x = "Week")
p <- p + theme_bw(base_size = 20) 
#p <- p + geom_point()
p <- p + geom_smooth()
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=1)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=1)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=1)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=1)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + scale_y_continuous(limits = c(0,1))
#p <- p + facet_wrap(~ Species)
p 


#### mixed model - pike 
df.pike<-subset(df.high,fSpecies=="Northern Pike")
m1.pike <- glm(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week,
             data = df.pike,
             family = binomial)
summary(m1.pike) ##

m2.pike <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
            data = df.pike, 
            family = binomial)
#, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(m2.pike) ##
r2(m2.pike)

new.pike <- expand.grid(Week = unique(df.pike$Week),
                        fYear = unique(df.pike$fYear))
new.pike$predicted_prob <- predict(m1.pike, new.pike, type = "response", re.form = NA) # use re.form = NULL if including random effects
new.pike$predicted_prob.2 <- predict(m2.pike, new.pike, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Northern Pike")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.pike, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.pike, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_line(data=new.pike, 
#                   aes(x = Week, y = predicted_prob.2),colour="darkorange",linewidth=2)
#p <- p + geom_smooth(data=df.pike,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_NorthernPike_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Bowfin
df.bowf<-subset(df.high,fSpecies=="Bowfin")
m1.bowf <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                   data = df.bowf, 
                   family = binomial)
summary(m1.bowf) ##
r2(m1.bowf)

new.bowf <- expand.grid(Week = unique(df.bowf$Week), fYear = unique(df.bowf$fYear))
new.bowf$predicted_prob <- predict(m1.bowf, new.bowf, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Bowfin")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.bowf, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.bowf, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.bowf,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_Bowfin_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()


#### mixed model - Brown Bullhead
df.brbull<-subset(df.high,fSpecies=="Brown Bullhead")
m1.brbull <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                 data = df.brbull, 
                 family = binomial)
summary(m1.brbull) ##
r2(m1.brbull)
m2.brbull <- glm(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week,
               data = df.brbull,
               family = binomial)
summary(m2.brbull) ##

new.brbull <- expand.grid(Week = unique(df.brbull$Week), fYear = unique(df.brbull$fYear))
new.brbull$predicted_prob <- predict(m1.brbull, new.brbull, type = "response", re.form = NA) # use re.form = NULL if including random effects
new.brbull$predicted_prob.2 <- predict(m2.brbull, new.brbull, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Brown Bullhead")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.brbull, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.brbull, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_line(data=new.brbull, 
#                   aes(x = Week, y = predicted_prob.2),colour="darkorange",linewidth=2)
#p <- p + geom_smooth(data=df.brbull,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_BrownBullhead_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()





#### mixed model - Channel Catfish
df.chcat<-subset(df.high,fSpecies=="Channel Catfish")
m1.chcat <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                   data = df.chcat, 
                   family = binomial)
summary(m1.chcat) ##
r2(m1.chcat)
m2.chcat <- glm(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week,
                 data = df.chcat,
                 family = binomial)
summary(m2.chcat) ##

new.chcat <- expand.grid(Week = unique(df.chcat$Week), fYear = unique(df.chcat$fYear))
new.chcat$predicted_prob <- predict(m1.chcat, new.chcat, type = "response", re.form = NA) # use re.form = NULL if including random effects
new.chcat$predicted_prob.2 <- predict(m2.chcat, new.chcat, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Channel Catfish")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.chcat, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.chcat, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
p <- p + geom_line(data=new.chcat, 
                   aes(x = Week, y = predicted_prob.2),colour="darkorange",linewidth=2)
#p <- p + geom_smooth(data=df.chcat,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_ChannelCatfish_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Common Carp
df.ccarp<-subset(df.high,fSpecies=="Common Carp")
m1.ccarp <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.ccarp, 
                  family = binomial)
summary(m1.ccarp) ##
r2(m1.ccarp)

new.ccarp <- expand.grid(Week = unique(df.ccarp$Week), fYear = unique(df.ccarp$fYear))
new.ccarp$predicted_prob <- predict(m1.ccarp, new.ccarp, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Common Carp")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.ccarp, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.ccarp, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.ccarp,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_CommonCarp_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()


#### mixed model - Common Carp x Goldfish
df.hybrids<-subset(df.high,fSpecies=="Common Carp x Goldfish")
m1.hybrids <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.hybrids, 
                  family = binomial)
summary(m1.hybrids) ##
r2(m1.hybrids)

new.hybrids <- expand.grid(Week = unique(df.hybrids$Week), fYear = unique(df.hybrids$fYear))
new.hybrids$predicted_prob <- predict(m1.hybrids, new.hybrids, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Common Carp x Goldfish")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.hybrids, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.hybrids, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.hybrids,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_CommonCarpxGoldfish_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()


#### mixed model - Freshwater Drum
df.fdrum<-subset(df.high,fSpecies=="Freshwater Drum")
m1.fdrum <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.fdrum, 
                  family = binomial)
summary(m1.fdrum) ##
r2(m1.fdrum)

new.fdrum <- expand.grid(Week = unique(df.fdrum$Week), fYear = unique(df.fdrum$fYear))
new.fdrum$predicted_prob <- predict(m1.fdrum, new.fdrum, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Freshwater Drum")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.fdrum, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.fdrum, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.fdrum,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_FreshwaterDrum_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Gizzard Shad
df.gshad<-subset(df.high,fSpecies=="Gizzard Shad")
m1.gshad <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.gshad, 
                  family = binomial)
summary(m1.gshad) ##
r2(m1.gshad)

new.gshad <- expand.grid(Week = unique(df.gshad$Week), fYear = unique(df.gshad$fYear))
new.gshad$predicted_prob <- predict(m1.gshad, new.gshad, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Gizzard Shad")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.gshad, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.gshad, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.gshad,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_GizzardShad_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Goldfish
df.gfish<-subset(df.high,fSpecies=="Goldfish")
m1.gfish <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.gfish, 
                  family = binomial)
summary(m1.gfish) ##
r2(m1.gfish)

new.gfish <- expand.grid(Week = unique(df.gfish$Week), fYear = unique(df.gfish$fYear))
new.gfish$predicted_prob <- predict(m1.gfish, new.gfish, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Goldfish")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.gfish, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.gfish, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.gfish,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_Goldfish_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Largemouth Bass
df.lbass<-subset(df.mod,fSpecies=="Largemouth Bass")
m1.lbass <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.lbass, 
                  family = binomial)
summary(m1.lbass) ##
r2(m1.lbass)

new.lbass <- expand.grid(Week = unique(df.lbass$Week), fYear = unique(df.lbass$fYear))
new.lbass$predicted_prob <- predict(m1.lbass, new.lbass, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Largemouth Bass")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.lbass, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.lbass, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.lbass,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_LargemouthBass_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()



#### mixed model - Rainbow Trout
df.rtrout<-subset(df.high,fSpecies=="Rainbow Trout")
m1.rtrout <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.rtrout, 
                  family = binomial)
summary(m1.rtrout) ##
r2(m1.rtrout)

new.rtrout <- expand.grid(Week = unique(df.rtrout$Week), fYear = unique(df.rtrout$fYear))
new.rtrout$predicted_prob <- predict(m1.rtrout, new.rtrout, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Rainbow Trout")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.rtrout, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.rtrout, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.rtrout,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_RainbowTrout_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Rudd
df.rudd<-subset(df.mod,fSpecies=="Rudd")
m1.rudd <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                   data = df.rudd, 
                   family = binomial)
summary(m1.rudd) ##
r2(m1.rudd)

new.rudd <- expand.grid(Week = unique(df.rudd$Week), fYear = unique(df.rudd$fYear))
new.rudd$predicted_prob <- predict(m1.rudd, new.rudd, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Rudd")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.rudd, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.rudd, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.rudd,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_Rudd_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - White Bass
df.wbass<-subset(df.mod,fSpecies=="White Bass")
m1.wbass <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                 data = df.wbass, 
                 family = binomial)
summary(m1.wbass) ##
r2(m1.wbass)

new.wbass <- expand.grid(Week = unique(df.wbass$Week), fYear = unique(df.wbass$fYear))
new.wbass$predicted_prob <- predict(m1.wbass, new.wbass, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="White Bass")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.wbass, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.wbass, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.wbass,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_WhiteBass_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - White Perch
df.wperch<-subset(df.high,fSpecies=="White Perch")
m1.wperch <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.wperch, 
                  family = binomial)
summary(m1.wperch) ##
r2(m1.wperch)

new.wperch <- expand.grid(Week = unique(df.wperch$Week), fYear = unique(df.wperch$fYear))
new.wperch$predicted_prob <- predict(m1.wperch, new.wperch, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="White Perch")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.wperch, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.wperch, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.wperch,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_WhitePerch_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - White Sucker
df.wsuck<-subset(df.high,fSpecies=="White Sucker")
m1.wsuck <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                   data = df.wsuck, 
                   family = binomial)
summary(m1.wsuck) ##
r2(m1.wsuck)

new.wsuck <- expand.grid(Week = unique(df.wsuck$Week), fYear = unique(df.wsuck$fYear))
new.wsuck$predicted_prob <- predict(m1.wsuck, new.wsuck, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="White Sucker")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.wsuck, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.wsuck, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.wsuck,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_WhiteSucker_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

#### mixed model - Yellow Perch
df.yperch<-subset(df.high,fSpecies=="Yellow Perch")
m1.yperch <- glmer(cbind(cumul.catch, Year.Sum - cumul.catch) ~ Week + (1 | fYear), 
                  data = df.yperch, 
                  family = binomial)
summary(m1.yperch) ##
r2(m1.yperch)

new.yperch <- expand.grid(Week = unique(df.yperch$Week), fYear = unique(df.yperch$fYear))
new.yperch$predicted_prob <- predict(m1.yperch, new.yperch, type = "response", re.form = NA) # use re.form = NULL if including random effects

p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week",title="Yellow Perch")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_jitter(data = df.yperch, 
                     aes(x = Week, y = cumul.prop), 
                     width = 0.2, height = 0.02, alpha = 0.3)
p <- p + geom_line(data=new.yperch, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
#p <- p + geom_smooth(data=df.yperch,aes(x=Week,y=cumul.prop))
p 

png("Fishway_CumulProp_YellowPerch_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()



## Combo plot for select species
p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_line(data=new.pike, 
                   aes(x = Week, y = predicted_prob),colour="brown",linewidth=2)
p <- p + geom_line(data=new.wsuck, 
                   aes(x = Week, y = predicted_prob),colour="darkgrey",linewidth=2)
p <- p + geom_line(data=new.chcat, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
p <- p + geom_line(data=new.bowf, 
                   aes(x = Week, y = predicted_prob),colour="darkgreen",linewidth=2)
p 



## Combo plot for cold and coolwater species
p <- ggplot()
p <- p + labs(y= "Cumulative Prop. Annual Total Catch", x = "Week")
p <- p + theme_bw(base_size = 20) 
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + annotate("rect", xmin = 10.5, xmax = 21.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#0072B2")
p <- p + annotate("rect", xmin = 17.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#D55E00")
p <- p + geom_line(data=new.pike, 
                   aes(x = Week, y = predicted_prob),colour="brown",linewidth=2)
p <- p + geom_line(data=new.wsuck, 
                   aes(x = Week, y = predicted_prob),colour="darkgrey",linewidth=2)
p <- p + geom_line(data=new.yperch, 
                   aes(x = Week, y = predicted_prob),colour="black",linewidth=2)
p <- p + geom_line(data=new.rudd, 
                   aes(x = Week, y = predicted_prob),colour="darkgreen",linewidth=2)
p <- p + geom_line(data=new.rtrout, 
                   aes(x = Week, y = predicted_prob),colour="darkblue",linewidth=2)
p 

png("Fishway_ColdCoolSpecies_03July2025.png",
    width = 2400, height = 2400,units="px",res=300)
p
dev.off()

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
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
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
p <- p +  geom_vline(xintercept=10.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=21.5, linetype="dashed",colour="blue", linewidth=2)
p <- p +  geom_vline(xintercept=17.5, linetype="dotdash",colour="red", linewidth=2)
p <- p +  geom_vline(xintercept=28.5, linetype="dotdash",colour="red", linewidth=2)
p <- p + geom_smooth(colour = "black")
p <- p + facet_wrap(~ Species)
p 

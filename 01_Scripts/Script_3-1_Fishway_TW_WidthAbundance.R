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

###############
## Functions ##
###############
time.window <- function(data) {
  Time.Min<-aggregate(YMD~Species+Year,data,min)
  Time.Max<-aggregate(YMD~Species+Year,data,max)
  Tracking.Window<-merge(Time.Min,Time.Max,by=c("Species","Year"))
  Tracking.Window$Time.Diff<-Tracking.Window$YMD.y-Tracking.Window$YMD.x
  Tracking.Window<-plyr::rename(Tracking.Window,c("YMD.x"="1st Detection","YMD.y"="Last Detection","Time.Diff"="Tracking Window"))
  Tracking.Window ## Check for errors ##
}

time.wk.window <- function(data) {
  Time.Min<-aggregate(Week~Species+fYear,data,min)
  Time.Max<-aggregate(Week~Species+fYear,data,max)
  Tracking.Window<-merge(Time.Min,Time.Max,by=c("Species","fYear"))
  Tracking.Window$Time.Diff<-Tracking.Window$Week.y-Tracking.Window$Week.x
  Tracking.Window<-plyr::rename(Tracking.Window,c("Week.x"="1st Detection","Week.y"="Last Detection","Time.Diff"="Tracking Window"))
  Tracking.Window ## Check for errors ##
}

## import required datasets
df.wk.plot <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_WeeklyMeans_by_Species_18June2025.rds")
df.3 <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_BaseDataset_18June2025.rds")
year.sum <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_YearlySum_by_Species_18June2025.rds")
sum.species <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_SumBySpecies_18June2025.rds")
setwd("~/github/Timing-Windows/03_Output/")



###############
## Timing window width and abundance
################
## daily summaries
#Weekly dataset
df.day.Years<-aggregate(Quantity~Year+Day+YMD,data=df.3,FUN=sum) ## total catch in a day?
df.species<-c(unique(df.3$Species)) ## list of species
df.day.Years.rep<-df.day.Years[rep(seq_len(nrow(df.day.Years)), length(df.species)), ] ## rep by # species
df.species.rep<-rep(df.species,nrow(df.day.Years)) ## rep species by year
df.day.Years.rep <-df.day.Years.rep[order(df.day.Years.rep$Year, df.day.Years.rep$Day),] ## order years
df.day.base<-data.frame(df.day.Years.rep,df.species.rep) ## combine year and species
df.day.base<-plyr::rename(df.day.base,c("df.species.rep"="Species"))
df.day.base$Quantity=NULL ## create empty dataset with every day when sampling occurred

df.day<-aggregate(Quantity~Species+Year+Day+YMD,data=df.3,FUN=sum) ## summarize catch by day-year

df.a<-merge(df.day,sum.species,by=c("Species"))

df.b<-subset(df.a,Rank=="High")
df.b<-plyr::rename(df.b,c("Quantity.x"="Day.Sum","Quantity.y"="Year.Sum"))

df.year<-aggregate(Quantity~Species+Year,data=df.3,FUN=sum) ## summarize catch by year
#
df.d.temp<-merge(df.day.base,df.day,by=c("Year","Day","Species","YMD"),all=T) ## combine dayly catch
df.d.temp[is.na(df.d.temp)] = 0 
#
df.d.plot<-merge(df.d.temp,df.year,by=c("Species","Year"),all=T) ## combine yearly
df.d.plot<-plyr::rename(df.d.plot,c("Quantity.x"="Day.Sum","Quantity.y"="Year.Sum"))
df.d.plot[is.na(df.d.plot)] = 0 
#
df.d.plot$fYear<-as.factor(df.d.plot$Year)
df.d.plot$Prop.Year<-df.d.plot$Day.Sum/df.d.plot$Year.Sum
#
df.d.plot.a<-merge(df.d.plot,sum.species,by=c("Species"))
df.d.plot.a<-subset(df.d.plot.a,Rank=="High")
df.d.plot.a.01<-subset(df.d.plot.a,Prop.Year>=0.01) ## drop days with <1% catch

df.d.plot.a.01.window<-data.frame(time.window(df.d.plot.a.01)) 
df.d.plot.a.01.window$Window<-as.numeric(df.d.plot.a.01.window$Tracking.Window + 1) ## need to add 1 otherwise doesn't count the first day.

df.c<-merge(df.d.plot.a.01.window,year.sum,by=c("Species","Year"))
df.c$logCatch<-log10(df.c$TotalCatch)
df.c$fSpecies<-as.factor(df.c$Species)
df.c$nTotalCatch<-as.numeric(df.c$TotalCatch)
df.c$fYear<-as.factor(df.c$Year)
#saveRDS(df.c, file = "TW_Fishway_Window_Catch_19June2025.rds")


plot(Window~logCatch,data=df.c)
lm1<-lm(Window~logCatch,data=df.c)
#lm1<-glm(TotalCatch~Window*Species,data=df.c)
abline(lm1)
summary(lm1)
my.seg <- segmented(lm1, 
                    seg.Z = ~ logCatch, 
                    psi = list(logCatch = c(1,2.5)))
summary(my.seg)
# get the breakpoints
my.seg$psi
# get the slopes
slope(my.seg)
# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(logCatch = df.c$logCatch, Window = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = logCatch, y = Window)) + geom_line()

p <- ggplot(data=df.c,aes(x=logCatch,y=Window,color=Species))
p <- p +  labs(y= "Capture Window (days)", x = "log10(Annual Total Catch)")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p + xlim(0,5)
p <- p + ylim(0,200)
p <- p + geom_line(data = my.model, aes(x = logCatch, y = Window), colour = "tomato",lwd=2)
p 







# Calculate the mean and standard error
l.model <- lm(Day.Sum ~ 1, subset(df.b,Species=="Bowfin"))
plot(Day.Sum ~ Year, subset(df.b,Species=="Bowfin"))
abline(l.model)
# Calculate the confidence interval
confint(l.model, level=0.95)
plot_model(m1.2, type = "re")
ranef_data.2 <- ranef(m1.2, condVar = TRUE)
dotplot(ranef_data.2, scales = list(x = list(relation = "free")))















## plot relationships between total catch and window width
df.b.window<-data.frame(time.window(df.b))
df.b.window$Window<-as.numeric(df.b.window$Tracking.Window + 1) ## need to add 1 otherwise doesn't count the first day.

plot(Window~as.factor(Year),data=df.b.window)

df.c<-merge(df.b.window,year.sum,by=c("Species","Year"))
df.c$logCatch<-log10(df.c$TotalCatch)
dotchart(df.c$TotalCatch)
plot(Window~logCatch,data=df.c)
lm1<-lm(Window~logCatch,data=df.c)
#lm1<-glm(TotalCatch~Window*Species,data=df.c)
abline(lm1)
summary(lm1)

#lm2poly <- lm(Window ~ poly(log(TotalCatch), 2), data = df.c)
## https://rpubs.com/MarkusLoew/12164

# have to provide estimates for breakpoints.
# after looking a the data, 
my.seg <- segmented(lm1, 
                    seg.Z = ~ logCatch, 
                    psi = list(logCatch = c(1,2.5)))

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
#my.seg <- segmented(my.lm, 
#                    seg.Z = ~ DistanceMeters, 
#                    psi = NA)

# display the summary
summary(my.seg)
# get the breakpoints
my.seg$psi
# get the slopes
slope(my.seg)
# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(logCatch = df.c$logCatch, Window = my.fitted)

# plot the fitted model
ggplot(my.model, aes(x = logCatch, y = Window)) + geom_line()

p <- ggplot(data=df.c,aes(x=logCatch,y=Window,color=Species))
p <- p +  labs(y= "Capture Window (days)", x = "log10(Annual Total Catch)")
p <- p + theme_bw(base_size = 20) 
p <- p + geom_point()
p <- p + xlim(0,5)
p <- p + ylim(0,200)
p <- p + geom_line(data = my.model, aes(x = logCatch, y = Window), colour = "tomato",lwd=2)
p 


plot(Window~logCatch,data=subset(df.c,Species=="Common Carp"))
lm.carp<-lm(Window~logCatch,data=subset(df.c,Species=="Common Carp"))
abline(lm.carp)
summary(lm.carp)

plot(Window~logCatch,data=subset(df.c,Species=="Gizzard Shad"))
lm.giz<-lm(Window~logCatch,data=subset(df.c,Species=="Gizzard Shad"))
abline(lm.giz)
summary(lm.giz) #positive linear

plot(Window~logCatch,data=subset(df.c,Species=="Brown Bullhead"))
lm.bb<-lm(Window~logCatch,data=subset(df.c,Species=="Brown Bullhead"))
abline(lm.bb)
summary(lm.bb)

plot(Window~logCatch,data=subset(df.c,Species=="White Sucker"))
lm.ws<-lm(Window~logCatch,data=subset(df.c,Species=="White Sucker"))
abline(lm.ws)
summary(lm.ws)

plot(Window~logCatch,data=subset(df.c,Species=="Goldfish"))
lm.gf<-lm(Window~logCatch,data=subset(df.c,Species=="Goldfish"))
abline(lm.gf)
summary(lm.gf) #positive linear

plot(Window~logCatch,data=subset(df.c,Species=="Northern Pike"))
lm.pike<-lm(Window~logCatch,data=subset(df.c,Species=="Northern Pike"))
abline(lm.pike)
summary(lm.pike) #positive linear

plot(Window~logCatch,data=subset(df.c,Species=="Yellow Perch"))
lm.perch<-lm(Window~logCatch,data=subset(df.c,Species=="Yellow Perch"))
abline(lm.perch)
summary(lm.perch)







carp<-subset(df.b,Species=="Common Carp")
ggplot(data=carp, aes(x=Day, group=Day.Sum, fill=Day.Sum)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~Year) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )


# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(carp, aes(x = Day, y = Year, fill = Day.Sum)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


dens <- density(subset(carp,Year==2008)$Day.Sum)


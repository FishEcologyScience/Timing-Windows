rm(list = ls())
###
library(plyr)
library(lubridate)
library(reshape)
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(performance)
library(lsmeans)
library(multcomp)
library(ggeffects)


## load data
df <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_Window_Catch_03July2025.rds")
head(df) ## base dataset
setwd("~/github/Timing-Windows/03_Output/")

m1<-lmer(Window ~ logCatch + (1 | fSpecies) ,data=df, REML=T) # only random intercept by species
summary(m1) ## p=0.0137
m1.2 <- lmer(Window ~ logCatch + (logCatch | fSpecies) ,data=df, REML=T) ## random slope and intercept by species 
summary(m1.2) # p=0.165 ## most appropriate model? 
m2<-lmer(Window ~ logCatch + (logCatch | fSpecies) + (1 | fYear),data=df, REML=T) #
summary(m2) #p=0.024
plot(m1.2)

## plot overall fixed effect
pred <- ggpredict(m1.2, terms = "logCatch") ## fixed effect
p1<-ggplot(pred, aes(x = x, y = predicted)) + 
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "log10(Annual Total Catch)", y = "Capture Duration (days)") +
  theme_bw(base_size = 20) 
p1
png("Window_By_TotalCatch_GLMM_FixedEffect_03July2025.png",
    width = 2000, height = 2000,units="px",res=300)
p1
dev.off()

## overall effect
df <- df %>%mutate(predicted = predict(m1.2))
p2<-ggplot(df, aes(x = logCatch, y = Window, color = fSpecies)) +
  theme_bw(base_size = 20) + 
  geom_point(alpha = 0.6) +
  geom_line(aes(y = predicted, group = fSpecies), size = 1) +
  labs(y= "Capture Duration (days)", x = "log10(Annual Total Catch)",color="Species")
p2
png("Window_By_TotalCatch_GLMM_MainPlot_03July2025.png",
    width = 2800, height = 2000,units="px",res=300)
p2
dev.off()

# random effects
ranef_data <- ranef(m1.2, condVar = TRUE)
p3<-dotplot(ranef_data, scales = list(x = list(relation = "free")))
p3
png("Window_By_TotalCatch_GLMM_RandomSlopeIntercept_03July2025.png",
    width = 2000, height = 2000,units="px",res=300)
p3
dev.off()



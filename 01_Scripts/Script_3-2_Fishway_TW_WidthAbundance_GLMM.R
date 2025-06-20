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
df <- readRDS("~/github/Timing-Windows/02_Data/TW_Fishway_Window_Catch_19June2025.rds")
head(df) ## base dataset

m1<-lmer(Window ~ logCatch + (1 | fSpecies) ,data=df, REML=T) # only random intercept by species
summary(m1) ## p<0.0001
m1.2 <- lmer(Window ~ logCatch + (logCatch | fSpecies) ,data=df, REML=T) ## random slope and intercept by species 
summary(m1.2) # p=0.0009 ## most appropriate model? 
m2<-lmer(Window ~ logCatch + (logCatch | fSpecies) + (1 | fYear),data=df, REML=T) #
summary(m2) #p=0.0002
plot(m1.2)

## plot overall fixed effect
pred <- ggpredict(m1.2, terms = "logCatch") ## fixed effect
ggplot(pred, aes(x = x, y = predicted)) + 
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "log10(Annual Total Catch)", y = "Predicted Capture Window (weeks)") +
  theme_bw(base_size = 20) 

## overall effect
df <- df %>%mutate(predicted = predict(m1.2))
ggplot(df, aes(x = logCatch, y = Window, color = fSpecies)) +
  theme_bw(base_size = 20) + 
  geom_point(alpha = 0.6) +
  geom_line(aes(y = predicted, group = fSpecies), size = 1) +
  labs(y= "Capture Window (weeks)", x = "log10(Annual Total Catch)",color="Species")

# random effects
ranef_data <- ranef(m1.2, condVar = TRUE)
dotplot(ranef_data, scales = list(x = list(relation = "free")))



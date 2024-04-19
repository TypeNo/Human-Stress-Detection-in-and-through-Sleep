library(AICcmodavg)
library(base)
library(broom)
library(datasets)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggpubr)
library(graphics)
library(grDevices)
library(methods)
library(readr)
library(readxl)
library(tidyverse)
library(webr)
##Anova Test
ANOVA_project <- read_excel("ANOVA project.xlsx",range = "G1:H51")
data_aov<- aov(ANOVA_project$`Heart Rate`~factor(ANOVA_project$`Stress Level`))
summary(data_aov)

##1 sample hypothesis test
SaYoPillow2 <- read_excel("SaYoPillow2.xlsx",range = "A1:I631")
t=t.test(SaYoPillow2$`eye movement`,mu=90,data=asc,conf.level=0.95,alternative="greater")
plot(t)

## correlation graph
x<-SaYoPillow2$`blood oxygen levels`
y<-SaYoPillow2$`respiration rate`
plot(x,y,xlab="Blood Oxygen Level",ylab="Respiration Rate")
abline(lm(y~x),col="red",lwd=3)

##Regression Graph
x<-SaYoPillow2$`blood oxygen levels`
y<-SaYoPillow2$`respiration rate`
plot(x,y,xlab="Blood Oxygen Level",ylab="Respiration Rate")
abline(lm(y~x),col="red",lwd=3)
x<- SaYoPillow2$`sleeping hours`
y<- SaYoPillow2$`Stress Levels`
model<-lm(y~x)
model
plot(x,y)
abline(model)
summary(model)


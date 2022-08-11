########################################################################
#####################################################################
#####################################################################
## Name: Fitting LMM models for the FEV data, and getting individual predictions
## Author: Elizabeth Juarez
## PI: Kittelson, A and Stevens-Lapsley
## Project: Predicting based on XX
## Date: April 2020
## Update: June 2020 - using dataset with PEX removed
## Update: Aug 2020 - using the 2020 data pull (keeping all FEV measures)
## Update: 
##
## Description: 
##			  
##
## Components:
## 		- 
##		  
## Input:
## 		- 
## 		- 
## Output:
##		- 
##                     
##                     
##                     
## Notes:
## 		- 
##                
##              - 
##                
##              - 



## install.packages("segmented")

## install.packages("pacman")
##install.packages('BiocManager')
##devtools::install_github('ck2136/PMMSKNN')
##devtools::install_github("stefvanbuuren/brokenstick")

## install.packages("geoR")
##library(geoR)
##install.packages("/Users/juarezce/Downloads/lmenssp_1.2.tar.gz",
##                 repos = NULL, type="source")

##install.packages("/Users/juarezce/Downloads/geoR_1.8-1.tar.gz",
##                 repos = NULL, type="source")

##library(lmenssp)

## time is not
rm(list=ls())
getwd()

  
##setwd(dirg)
getwd()

##library("pacman")
##require(graphics)

##p_load(PMMSKNN, dplyr, here)
library(dplyr)
##library(segmented)

##library(GLMMadaptive)
library(JMbayes)

#### R script for CFFPR application ####
#load packages

##load for models fitting segmented lme modelwith Changepoints
library(nlme)
library(lme4)

################################################################################################
### reading the dataset
## --------------------------------------------------------------------------------------------
## train=2 corresponds to testing set
## agen=age from 0= time from 0. Dist from 0 through 15 years (mean=6.2)
## mut= genotype where 1= TWO alleles; 2= 1 allele; and 3= no allele or unknown
## sexf= 1 if female; 0 if male
fevd1 <- read.csv("data/FEV-data-train-test.csv")


names(fevd1)[names(fevd1) == 'age.min'] <- 'age.first'


head(fevd1)
## number of rows in dataset
nrow(fevd1)
## number of unique IDS
length(unique(fevd1$SysID)) ##343
summary(fevd1)

fevd1$time <- fevd1$agen
fevd1$mut <- as.factor(fevd1$mut)
fevd1$id <- as.factor(fevd1$id)

nrow(fevd1)
### data for model development
data.dev <- fevd1[fevd1$train==1,]
head(data.dev)
nrow(data.dev)

### data for TESTING model
data.test <- fevd1[fevd1$train==2,]
head(data.test)
nrow(data.test)
##names(data.test)[names(data.test) == "fev"] <- "fev.obs"
##data.test$fev <- NA

### data for TESTING model
## 
fevd1[fevd1$id==19,]



################################################################################################
### fitting LMM
## --------------------------------------------------------------------------------------------
## 

lm(fev ~ time + sexf + mut, data = data.dev)

### using lme4 package
fit0 <- lmer(fev ~ time + sexf + mut + (1 | id), data = data.dev)
summary(fit0)
## this produces population predictions - not individualized
data.test$fev.pred <- predict(fit0, newdata=data.test, type = 'response', allow.new.levels=TRUE)
head(data.test)

## example from R

## linear mixed model fit - EXAMPLE
data(pbc)
library(splines)
head(pbc2,2)
fitLME <- lme(log(serBilir) ~ drug * ns(year, 2), data = subset(pbc2, id != 2),random = ~ year | id)
DF <- IndvPred_lme(fitLME, newdata = subset(pbc2, id == 2 ), timeVar = "year", M = 500, return_data = TRUE)
View(DF)


## --------------------------------------------------------------------------------------------
### fixed factors for model
## --------------------------------------------------------------------------------------------
## fixed factors for model
ffactors <- c("sexf" , "mut" , "age.first" , "caucasian" , "fev0")
## fixed factors for model - selected
##ffactors.sel <- c("age.first" , "fev0")
ffactors.sel <- c("age.first" )
ffactors <- ffactors.sel

### using nlme package
## model with random intercepts only and a linear function of time
fit00 <- lme(as.formula(paste("fev~ time +", paste(ffactors, collapse="+"))), random= ~ 1 | id, data = data.dev)
summary(fit00)
summary(fit00)$AIC 
fit00 <- lme(fev ~ time  + age.first , random= ~ 1 | id, data = data.dev)
summary(fit00)
summary(fit00)$AIC 
fit00 <- lme(fev ~ time  + age.first + fev0, random= ~ 1 | id, data = data.dev)
summary(fit00)
summary(fit00)$AIC 


## model with random slopes and a DF=2 (quadratic) spline function of time
##fit0 <- lme(fev ~ ns(time, knots=c(5,10), df=2)  + age.first  + fev0, random= ~ time | id, data = data.dev)
fit0 <- lme(as.formula(paste("fev~ ns(time, knots=c(5,8), df=2) +", paste(ffactors.sel, collapse="+"))),
            random= ~ time | id, data = data.dev)
summary(fit0) 
summary(fit0)$AIC

## model with random slopes and a DF=1 (linear) spline function of time
##fit1 <- lme(fev ~   + age.first  + fev0, random= ~ time | id, data = data.dev)
fit1 <- lme(as.formula(paste("fev~ ns(time, knots=c(5,8), df=1) +", paste(ffactors.sel, collapse="+"))),
            random= ~ time | id, data = data.dev)
summary(fit1) 
summary(fit1)$AIC 

## model with random slopes and a DF=2 spline function of time - no knots
##fit2 <- lme(fev ~ ns(time, df=2)  + age.first  + fev0, random= ~ time | id, data = data.dev)
fit2 <- lme(as.formula(paste("fev~ ns(time, df=2) +", paste(ffactors.sel, collapse="+"))),
            random= ~ time | id, data = data.dev)
summary(fit2) 
summary(fit2)$AIC ## worse than simple piecewise constant


## df= 3 does not converge
##fit3 <- lme(fev ~ ns(time, df=3) + sexf + mut, random= ~ time | id, data = data.dev)
## CHOSEN model

## model with random slopes and a DF=1 (linear) spline function of time with knots at different places
##fit3 <- lme(fev ~ ns(time, knots=c(6,10), df=1)   + age.first  + fev0, random= ~ time | id, data = data.dev)
##fit3 <- lme(as.formula(paste("fev~ ns(time, knots=c(4,8), df=1) +", paste(ffactors.sel, collapse="+"))),
fit3 <- lme(as.formula(paste("fev~ ns(time, knots=c(6,9), df=1) +", paste(ffactors.sel, collapse="+"))),
            random= ~ time | id, data = data.dev)
summary(fit3) 
summary(fit3)$AIC 
## fit3 provides the same AIC than fit1, so I'll keep fit1


summary(fit00)$AIC 
summary(fit0)$AIC
summary(fit1)$AIC ## linear spline function of time
summary(fit2)$AIC ## worse than simple piecewise constant
summary(fit3)$AIC 


fit <- fit1
summary(fit) 
summary(fit)$AIC 

################################################################################################
### Calculating individualized predictions 
## --------------------------------------------------------------------------------------------
## 

## testing the prediction function on data used to develop the model - JUST as an example
##head(data.dev, 30)
##unique(data.dev$id)
##data.dev.pred <- IndvPred_lme(fit, newdata = subset(data.dev, (id==2 | id==3) & time == 0), timeVar = "time", M=300)
##data.dev.pred <- data.frame(data.dev.pred)
##head(data.dev.pred)
##names(data.dev.pred)  <- c("time", "fev.pred", "fev.ll", "fev.ul")
##str(data.dev.pred)

##head(data.test)


############## 
## producing indvidual predictions on the TEST dataset
## level= confidence level for prediction intervals
## note that am predicting for every individual at ALL unique times, but there may be a more efficient way of doing this
##data.test.pred <- IndvPred_lme(fit, newdata = subset(data.test, (id==1 | id==10)& time == 0), timeVar = "time", M=300,
pred <- IndvPred_lme(fit, newdata = subset(data.test, time == 0), timeVar = "time", M=300,
                     times=unique(data.test$time), 
                     return_data=TRUE, level=0.5, interval='prediction', seed=2423)
head(pred, 30)
head(data.test)


?IndvPred_lme


##pred$time
data.test$time[data.test$id==19]

data.test[data.test$id==1, ]
data.test[data.test$id==19, ]
data.test[data.test$id==39, ]
data.test[data.test$id==42, ]

pred[pred$id==19,]
pred[pred$id==39,]
pred[pred$id==42,]
unique(pred$id)
unique(data.test$id)



######
## merging with test dataset to ADD observed FEV
names(pred)[names(pred) == 'fev'] <- 'fev.bas' ## fev at time==0 (fev.baseline)
data.test.pred <- merge(data.test[ ,c('id','time','fev')], pred,
                     by.x=c('id','time'),
                     by.y=c('id','time'),
                     all.x=TRUE)
data.test.pred <- data.test.pred[order(data.test.pred$id, data.test.pred$time), ]
nrow(data.test)
nrow(data.test.pred) ## this matches up
head(data.test.pred)
summary(data.test.pred$fev)
summary(data.test.pred$pred)
##data.test.pred[is.na(data.test.pred$pred),][1:20,]
## ids with missing
##unique(data.test.pred[is.na(data.test.pred$pred),][1:20,]$id)


unique(data.test.pred$id)[1:20]
unique(data.test.pred$id)[1:20]

############## 
## producing indvidual predictions on the TRAIN dataset
## level= confidence level for prediction intervals
## note that am predicting for every individual at ALL unique times, 
## but there may be a more efficient way of doing this
dev.pred <- IndvPred_lme(fit, 
                         newdata = subset(data.dev, time == 0), 
                         timeVar = "time",
                         M=300,
                         times=unique(data.dev$time),
                         return_data=TRUE, 
                         level=0.5, 
                         interval = 'prediction', 
                         seed = 2423)

View(dev.pred)


## merging with test dataset to ADD observed FEV
names(dev.pred)[names(dev.pred) == 'fev'] <- 'fev.bas'
data.dev.pred <- merge(data.dev[ ,c('id','time','fev')], dev.pred,
                     by.x=c('id','time'),
                     by.y=c('id','time'),
                     all.x=TRUE)
data.dev.pred <- data.dev.pred[order(data.dev.pred$id, data.dev.pred$time), ]
nrow(data.dev)
nrow(data.dev.pred) ## 
head(data.dev.pred, 50)


head(data.dev.pred, 2)
head(data.test.pred, 2)

names(data.dev.pred)
names(data.test.pred) ## recall that fev.pred is the population prediction 


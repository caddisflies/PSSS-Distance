#Load R libraries and functions
#=====================================================
# Date          Modification
#----------------------------------------------------
# 2016 Feb 13    Original coding  - J Jannot
# 2018 May 11    Modified for Audobon distance data
#=====================================================

#clear the console
cat("\014")

#- Load libraries
#library(plyr)
#library(data.table)
library(ggplot2)
#library(logging)
#library(glmnet)
#library(pls)
#library(zoo)
#library(MASS)
#library(foreign)
#library(lattice)
#library(lme4)
#library(Hmisc)
#library(boot)
#library(reshape2)
#library(dataframes2xls)
#library(lubridate) #does nice date handling
#library(caret)# ensemble modeling
#library(caretEnsemble)#ensemble modeling
#library(mlbench)
#library(pROC)
#library(rpart) #Regression and Classification Trees using algos in the pdf for this package
#library(caTools)
#library(nnet)
#library(C50) #Classification and Regression Trees using C50 algo
#library(partykit)
#library(gbm)#Gradient boosting models
#library(RCurl)
#library(RANN)
#library(htmltab)
library(stringr)
library(dplyr)
#library(RSQLite)
#library(RecordLinkage)
library(PBSmapping)
library(sp)
library(knitr)
library(rmarkdown)

drive1 <- "C:/Users/Banksiola/Documents/R/"
source(paste(drive1, "functions.R", sep=""))
source(paste(drive1, "match.f.r", sep=""))

Sys.sleep(2)
#clear the console
cat("\014")
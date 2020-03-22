##########################################################################
#Name: Sarah Van Alsten
#Date Created: March 21, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, survey, survival, survminer
#Purpose: Check assumptions for CoxPH models from 03212020_cox_regressions.R
#Last Update: March 22, 2020
###########################################################################
#open packages
#install.packages("survey")
#install.packages("tidyverse")
#install.packages("survival")
#install.packages("survminer") #for ggcoxdiagnostics
library(survey)
library(tidyverse)
library(survival)
library(survminer)

#instructions for use: run models in 03212020_cox_regressions.R
#first and then use this file to check assumptions

#Assumption 1: Proportional Hazards
#####################################################################
#All Cancers 
(ph.all.unadj <- cox.zph(all.unadj)) #meets assumption
(ph.all.adj <- cox.zph(all.adjusted)) #met for CRN, not globally

#Breast Cancer
(ph.br.unadj <- cox.zph(br.unadj)) #meets assumption
(ph.br.adj <- cox.zph(br.adjusted)) #meets for CRN and globally

#Prostate Cancer
(ph.pr.unadj <- cox.zph(pr.unadj)) #meets assumption
(ph.pr.adj <- cox.zph(pr.adjusted)) #met for CRN, not globally

#Lung Cancer
(ph.lu.unadj <- cox.zph(lu.unadj)) #meets assumption
(ph.lu.adj <- cox.zph(lu.adjusted)) #met for CRN, not globally

#Lymphoma
(ph.ly.unadj <- cox.zph(ly.unadj)) #doesn't meet assumption
(ph.ly.adj <- cox.zph(ly.adjusted)) #not met for CRN and not met globally

#Colorectal
(ph.cr.unadj <- cox.zph(cr.unadj)) #meets assumption
(ph.cr.adj <- cox.zph(cr.adjusted)) #met for CRN and met globally
#########################################################################
#Assumption 2: No influential observations (dfbeta values)
#########################################################################
#all included cancers
dfb.all.unadj <- residuals(all.unadj, "dfbeta")
plot(dfb.all.adj) #anything > 0.01

dfb.all.adj <- residuals(all.adjusted, "dfbeta")

#breast cancer
dfb.br.unadj <- residuals(br.unadj, "dfbeta")
plot(dfb.br.adj) #anything > 0.01

dfb.br.adj <- residuals(br.adjusted, "dfbeta")

#prostate cancer
dfb.pr.unadj <- residuals(pr.unadj, "dfbeta")
plot(dfb.pr.adj) #anything > 0.01

dfb.pr.adj <- residuals(pr.adjusted, "dfbeta")


#lymphoma
dfb.ly.unadj <- residuals(ly.unadj, "dfbeta")
plot(dfb.ly.adj) #anything > 0.01

dfb.ly.adj <- residuals(ly.adjusted, "dfbeta")

#lung cancer
dfb.lu.unadj <- residuals(lu.unadj, "dfbeta")
plot(dfb.lu.adj) #anything > 0.01

dfb.br.adj <- residuals(lu.adjusted, "dfbeta")


#colorectal cancer
dfb.cr.unadj <- residuals(cr.unadj, "dfbeta")
plot(dfb.cr.adj) #anything > 0.01

dfb.cr.adj <- residuals(cr.adjusted, "dfbeta")

#############################################################
#Assumption 3: Linearity of Log-Hazard
#################################################################
resMart <- residuals(fitCPH, type="martingale")
plot(dfSurv$X, resMart, main="Martingale-residuals for X",
     xlab="X", ylab="Residuen", pch=20)
lines(loess.smooth(dfSurv$X, resMart), lwd=2, col="blue")
legend(x="bottomleft", col="blue", lwd=2, legend="LOESS fit", cex=1.4)




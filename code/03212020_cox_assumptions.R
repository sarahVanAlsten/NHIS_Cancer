##########################################################################
#Name: Sarah Van Alsten
#Date Created: March 21, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, survey, survival, survminer
#Purpose: Check assumptions for CoxPH models from 03212020_cox_regressions.R
#Last Update: March 21, 2020
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
(ph.br.adj <- cox.zph(br.adjusted)) #meets for CRN, not globally

#Prostate Cancer
(ph.pr.unadj <- cox.zph(pr.unadj)) #meets assumption
(ph.pr.adj <- cox.zph(pr.adjusted)) #met for CRN, not globally

#Lung Cancer
(ph.lu.unadj <- cox.zph(lu.unadj)) #meets assumption
(ph.lu.adj <- cox.zph(lu.adjusted)) #met for CRN, not globally

#Lymphoma
(ph.ly.unadj <- cox.zph(ly.unadj)) #doesn't meet assumption
(ph.ly.adj <- cox.zph(ly.adjusted)) #not met for CRN, but met globally

#Colorectal
(ph.cr.unadj <- cox.zph(cr.unadj)) #meets assumption
(ph.cr.adj <- cox.zph(cr.adjusted)) #met for CRN and met globally
#########################################################################
#Assumption 2: No influential observations (dfbeta values)
#########################################################################
#all included cancers
dfb.all.unadj <- residuals(all.unadj, "dfbeta")
dfb.all.adj <- residuals(all.adjusted, "dfbeta")

#unadjusted
plot(dfb.all.unadj) #anything > 0.02 is outlier

#adjusted: one for each covariate
plot(dfb.all.adj[,1], ylab = names(all.adjusted$coefficients)[1]) #keep anything < abs (.02)
plot(dfb.all.adj[,2], ylab = names(all.adjusted$coefficients)[2]) #anything < abs(.005)
plot(dfb.all.adj[,3], ylab = names(all.adjusted$coefficients)[3]) #keep anything < abs(.04)
plot(dfb.all.adj[,4], ylab = names(all.adjusted$coefficients)[4]) #anything > -.03
plot(dfb.all.adj[,5], ylab = names(all.adjusted$coefficients)[5]) #anything > -.03
plot(dfb.all.adj[,6], ylab = names(all.adjusted$coefficients)[6]) #anything > - 0.02
plot(dfb.all.adj[,7], ylab = names(all.adjusted$coefficients)[7]) #keep anything > -.001
plot(dfb.all.adj[,8], ylab = names(all.adjusted$coefficients)[8]) #keep anything > -.01
plot(dfb.all.adj[,9], ylab = names(all.adjusted$coefficients)[9]) #keep anything < .02
plot(dfb.all.adj[,10], ylab = names(all.adjusted$coefficients)[10]) #keep anything < .02
plot(dfb.all.adj[,11], ylab = names(all.adjusted$coefficients)[11]) #keep anything < abs(.001)

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy.1 <- update(comp.svy, dfb.all.adj = as.numeric(dfb.all.unadj))
comp.svy.1 <- subset(comp.svy.1, dfb.all.unadj <= 0.02)

#run coxph and print results
all.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                      design = comp.svy.1)
summary(all.unadj.1)

#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy2 <- update(comp.svy, dfb.all.adj = dfb.all.adj)
comp.svy2 <- subset(comp.svy2,
                    dfb.all.adj[,1] < abs(.02),
                    dfb.all.adj[,2]< abs(.005),
                    dfb.all.adj[,3]< abs(.04),
                    dfb.all.adj[,4]> -.03,
                    dfb.all.adj[,5]> -.03,
                    dfb.all.adj[,6]> -.02,
                    dfb.all.adj[,7]> - 0.001,
                    dfb.all.adj[,8]> -.01,
                    dfb.all.adj[,9]< .02,
                    dfb.all.adj[,10] < .02,
                    dfb.all.adj[,11]< abs(.001))

#run coxph and print results
all.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                        factor(insurance_new) +
                        age_new + factor(race_new)+
                        yrs_any,
                        design = comp.svy2)
summary(all.adj.1)
#################################################################
#breast cancer
dfb.br.unadj <- residuals(br.unadj, "dfbeta")
dfb.br.adj <- residuals(br.adjusted, "dfbeta")

#unadjusted
plot(dfb.br.unadj) #anything > 0.1 is outlier

#adjusted: one for each covariate
plot(dfb.br.adj[,1], ylab = names(br.adjusted$coefficients)[1])    #keep all <.05
plot(dfb.br.adj[,2], ylab = names(br.adjusted$coefficients)[2])    #keep all < abs(.1)
plot(dfb.br.adj[,3], ylab = names(br.adjusted$coefficients)[3])    #keep all > -.05
plot(dfb.br.adj[,4], ylab = names(br.adjusted$coefficients)[4])    #keep all < abs(.1)
plot(dfb.br.adj[,5], ylab = names(br.adjusted$coefficients)[5])    #keep all < abs(.05)
plot(dfb.br.adj[,6], ylab = names(br.adjusted$coefficients)[6])    #keep all <abs(.001)
plot(dfb.br.adj[,7], ylab = names(br.adjusted$coefficients)[7])    #keep all <abs(.02)
plot(dfb.br.adj[,8], ylab = names(br.adjusted$coefficients)[8])    #keep all <.05
plot(dfb.br.adj[,9], ylab = names(br.adjusted$coefficients)[9])    #keep all <.05
plot(dfb.br.adj[,10], ylab = names(br.adjusted$coefficients)[10])  #keep all <abs(.005)

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy3 <- update(br.svy, dfb.br.adj = as.numeric(dfb.br.unadj))
comp.svy3 <- subset(comp.svy3, dfb.br.unadj <= 0.1)

#run coxph and print results
br.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                        design = comp.svy3)
summary(br.unadj.1) 
#how many died by CRN status
table(comp.svy3$variables$CRN, comp.svy3$variables$cancMort)

#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy4 <- update(br.svy, dfb.br.adj = dfb.br.adj)
comp.svy4 <- subset(comp.svy4,
                    dfb.br.adj[,1]<.1,
                    dfb.br.adj[,2]< abs(.1),
                    dfb.br.adj[,3] > -0.05,
                    dfb.br.adj[,4]< abs(.1),
                    dfb.br.adj[,5]< abs(.05),
                    dfb.br.adj[,6]< abs(.001),
                    dfb.br.adj[,7]< abs(.02),
                    dfb.br.adj[,8]< .05,
                    dfb.br.adj[,9]< .05,
                    dfb.br.adj[,10]<abs(.005))

#run coxph and print results 
br.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                       design = comp.svy4)
summary(br.adj.1)

#how many died by CRN status
table(comp.svy4$variables$CRN, comp.svy4$variables$cancMort)
##################################################################
#prostate cancer
dfb.pr.unadj <- residuals(pr.unadj, "dfbeta") 
dfb.pr.adj <- residuals(pr.adjusted, "dfbeta")

#which ones are outliers?
#unadjusted model
plot(dfb.pr.unadj)#keep everything < 0.05

#adjusted: one for each covariate
plot(dfb.pr.adj[,1], ylab = names(pr.adjusted$coefficients)[1])    #keep all < abs(.05)
plot(dfb.pr.adj[,2], ylab = names(pr.adjusted$coefficients)[2])    #keep all > -.1
plot(dfb.pr.adj[,3], ylab = names(pr.adjusted$coefficients)[3])    #keep all > -.1
plot(dfb.pr.adj[,4], ylab = names(pr.adjusted$coefficients)[4])    #keep all < abs(.1)
plot(dfb.pr.adj[,5], ylab = names(pr.adjusted$coefficients)[5])    #keep all < abs(.1)
plot(dfb.pr.adj[,6], ylab = names(pr.adjusted$coefficients)[6])    #keep all < abs(.001)
plot(dfb.pr.adj[,7], ylab = names(pr.adjusted$coefficients)[7])    #keep all < abs(.02)
plot(dfb.pr.adj[,8], ylab = names(pr.adjusted$coefficients)[8])    #keep all < abs(.05)
plot(dfb.pr.adj[,9], ylab = names(pr.adjusted$coefficients)[9])    #keep all <abs(.01)
plot(dfb.pr.adj[,10], ylab = names(pr.adjusted$coefficients)[10])  #keep all < abs(.004)

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy5 <- update(pr.svy, dfb.pr.adj = as.numeric(dfb.pr.unadj))
comp.svy5 <- subset(comp.svy5, dfb.pr.unadj <= 0.05)

#run coxph and print results
pr.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                       design = comp.svy5)
summary(pr.unadj.1) 
#CI is very wide: check cases who did by CRN... only 6
table(comp.svy5$variables$CRN, comp.svy5$variables$cancMort)

#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy6 <- update(pr.svy, dfb.pr.adj = dfb.pr.adj)
comp.svy6 <- subset(comp.svy6,
                    dfb.pr.adj[,1]< abs(.05),
                    dfb.pr.adj[,2] > -.1,
                    dfb.pr.adj[,3] > -.1,
                    dfb.pr.adj[,4] < abs(.1),
                    dfb.pr.adj[,5] < abs(.1),
                    dfb.pr.adj[,6] < abs(.001),
                    dfb.pr.adj[,7] < abs(.02),
                    dfb.pr.adj[,8] < abs(.05),
                    dfb.pr.adj[,9] < abs(.01),
                    dfb.pr.adj[,10] < abs(.004))

#run coxph and print results
pr.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                       design = comp.svy6)
summary(pr.adj.1)

#CI is very wide: check cases who did by CRN... only 7
table(comp.svy6$variables$CRN, comp.svy6$variables$cancMort)
###########################################################################
#lymphoma
dfb.ly.unadj <- residuals(ly.unadj, "dfbeta")
dfb.ly.adj <- residuals(ly.adjusted, "dfbeta")

#which ones are outliers?
#unadjusted model
plot(dfb.ly.unadj)#keep everything < 0.04

#adjusted: one for each covariate
plot(dfb.ly.adj[,1], ylab = names(ly.adjusted$coefficients)[1])    #keep all < abs(.05)
plot(dfb.ly.adj[,2], ylab = names(ly.adjusted$coefficients)[2])    #keep all < 0.05
plot(dfb.ly.adj[,3], ylab = names(ly.adjusted$coefficients)[3])    #keep all < abs(.2)
plot(dfb.ly.adj[,4], ylab = names(ly.adjusted$coefficients)[4])    #keep all < abs(.1)
plot(dfb.ly.adj[,5], ylab = names(ly.adjusted$coefficients)[5])    #keep all < abs(.1)
plot(dfb.ly.adj[,6], ylab = names(ly.adjusted$coefficients)[6])    #keep all >.09
plot(dfb.ly.adj[,7], ylab = names(ly.adjusted$coefficients)[7])    #keep all >-.002
plot(dfb.ly.adj[,8], ylab = names(ly.adjusted$coefficients)[8])    #keep all < abs(.05)
plot(dfb.ly.adj[,9], ylab = names(ly.adjusted$coefficients)[9])    #keep all <abs(.0015)
plot(dfb.ly.adj[,10], ylab = names(ly.adjusted$coefficients)[10])  #keep all < .1
plot(dfb.ly.adj[,11], ylab = names(ly.adjusted$coefficients)[11])  #keep all < abs(.009)

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy7 <- update(ly.svy, dfb.ly.adj = as.numeric(dfb.ly.unadj))
comp.svy7 <- subset(comp.svy7, dfb.ly.unadj <= 0.04)

#run coxph and lyint results
ly.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                       design = comp.svy7)
summary(ly.unadj.1) #converged too early, not enough cases

#see how many cases after exclusion:.. 0 that's a problem
table(comp.svy7$variables$CRN, comp.svy7$variables$cancMort)
#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy8 <- update(ly.svy, dfb.ly.adj = dfb.ly.adj)
comp.svy8 <- subset(comp.svy8,
                    dfb.ly.adj[,1]< abs(.05),
                    dfb.ly.adj[,2]  < 0.05,
                    dfb.ly.adj[,3] < abs(.2),
                    dfb.ly.adj[,4] < abs(.1),
                    dfb.ly.adj[,5] < abs(.1),
                    dfb.ly.adj[,6] > -.09,
                    dfb.ly.adj[,7] > - .002,
                    dfb.ly.adj[,8] < abs(.05),
                    dfb.ly.adj[,9] <abs(.001),
                    dfb.ly.adj[,10] < .1,
                    dfb.ly.adj[,11] < abs(009))

#run coxph and lyint results : after subsetting there
#weren't enough cases/contrasts to run model
ly.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                       design = comp.svy8)

summary(ly.adj.1) #again, not enough cases.. only 1
table(comp.svy8$variables$CRN, comp.svy8$variables$cancMort)
############################################################################
#lung cancer
dfb.lu.unadj <- residuals(lu.unadj, "dfbeta")
dfb.lu.adj <- residuals(lu.adjusted, "dfbeta")

#which ones are outliers?
#unadjusted model
plot(dfb.lu.unadj) #keep everything < abs(0.1)

#adjusted: one for each covariate
plot(dfb.lu.adj[,1], ylab = names(lu.adjusted$coefficients)[1])    #keep all < abs(.05)
plot(dfb.lu.adj[,2], ylab = names(lu.adjusted$coefficients)[2])    #keep all > -.02
plot(dfb.lu.adj[,3], ylab = names(lu.adjusted$coefficients)[3])    #keep all > -.02
plot(dfb.lu.adj[,4], ylab = names(lu.adjusted$coefficients)[4])    #keep all < abs(.1)
plot(dfb.lu.adj[,5], ylab = names(lu.adjusted$coefficients)[5])    #keep all >-.075
plot(dfb.lu.adj[,6], ylab = names(lu.adjusted$coefficients)[6])    #keep all < abs(.05)
plot(dfb.lu.adj[,7], ylab = names(lu.adjusted$coefficients)[7])    #keep all > -.003
plot(dfb.lu.adj[,8], ylab = names(lu.adjusted$coefficients)[8])    #keep all < abs(.02)
plot(dfb.lu.adj[,9], ylab = names(lu.adjusted$coefficients)[9])    #keep all <abs(.05)
plot(dfb.lu.adj[,10], ylab = names(lu.adjusted$coefficients)[10])  #keep all < .08
plot(dfb.lu.adj[,11], ylab = names(lu.adjusted$coefficients)[11])  #all good

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy9 <- update(lu.svy, dfb.lu.adj = as.numeric(dfb.lu.unadj))
comp.svy9 <- subset(comp.svy9, dfb.lu.unadj < abs(.1))

#run coxph and luint results
lu.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                       design = comp.svy9)
summary(lu.unadj.1) 

#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy10 <- update(lu.svy, dfb.lu.adj = dfb.lu.adj)
comp.svy10 <- subset(comp.svy10,
                    dfb.lu.adj[,1]< abs(.05),
                    dfb.lu.adj[,2] > -.02,
                    dfb.lu.adj[,3] > -.02,
                    dfb.lu.adj[,4] < abs(.1),
                    dfb.lu.adj[,5] >-.075,
                    dfb.lu.adj[,6] < abs(.05),
                    dfb.lu.adj[,7] > -.003,
                    dfb.lu.adj[,1] < abs(.02),
                    dfb.lu.adj[,8] <abs(.05),
                    dfb.lu.adj[,9] < .08,
                    dfb.lu.adj[,10] < abs(.1))

#run coxph and luint results : after subsetting there
#weren't enough cases/contrasts to run model
lu.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                       design = comp.svy10)
summary(lu.adj.1)

##################################################################################
#colorectal cancer
dfb.cr.unadj <- residuals(cr.unadj, "dfbeta")
dfb.cr.adj <- residuals(cr.adjusted, "dfbeta")

#which ones are outliers?
#unadjusted model
plot(dfb.cr.unadj)#keep everything < 0.01

#adjusted: one for each covariate
plot(dfb.cr.adj[,1], ylab = names(cr.adjusted$coefficients)[1])    #keep all < abs(.05)
plot(dfb.cr.adj[,2], ylab = names(cr.adjusted$coefficients)[2])    #keep all <.05
plot(dfb.cr.adj[,3], ylab = names(cr.adjusted$coefficients)[3])    #keep all < .1
plot(dfb.cr.adj[,4], ylab = names(cr.adjusted$coefficients)[4])    #keep all < abs(.1)
plot(dfb.cr.adj[,5], ylab = names(cr.adjusted$coefficients)[5])    #keep all < abs(.075)
plot(dfb.cr.adj[,6], ylab = names(cr.adjusted$coefficients)[6])    #keep all < abs(.1)
plot(dfb.cr.adj[,7], ylab = names(cr.adjusted$coefficients)[7])    #keep all < abs(.002)
plot(dfb.cr.adj[,8], ylab = names(cr.adjusted$coefficients)[8])    #keep all < abs(.05)
plot(dfb.cr.adj[,9], ylab = names(cr.adjusted$coefficients)[9])    #keep all <abs(.05)
plot(dfb.cr.adj[,10], ylab = names(cr.adjusted$coefficients)[10])  #keep all < abs(.1)
plot(dfb.cr.adj[,11], ylab = names(cr.adjusted$coefficients)[11])  #keep all < abs(.005)

#rerun the models to see changes
#1st the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy11 <- update(cr.svy, dfb.cr.adj = as.numeric(dfb.cr.unadj))
comp.svy11 <- subset(comp.svy11, dfb.cr.unadj <= 0.01)

#run coxph and crint results
cr.unadj.1 <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                       design = comp.svy11)
summary(cr.unadj.1) #converged too early, not enough cases; 0 in fact
table(comp.svy11$variables$CRN, comp.svy11$variables$cancMort)

#next the unadjusted model

#add the dfbeta values to svy object and subset out observations that don't
#meet the criteria
comp.svy12 <- update(cr.svy, dfb.cr.adj = dfb.cr.adj)
comp.svy12 <- subset(comp.svy12,
                    dfb.cr.adj[,1]< abs(.05),
                    dfb.cr.adj[,2] <.05,
                    dfb.cr.adj[,3] < .1,
                    dfb.cr.adj[,4] < abs(.1),
                    dfb.cr.adj[,5] < abs(.075),
                    dfb.cr.adj[,6] < abs(.1),
                    dfb.cr.adj[,7] < abs(.002),
                    dfb.cr.adj[,8] < abs(.05),
                    dfb.cr.adj[,9] <abs(.05),
                    dfb.cr.adj[,10] < abs(.1),
                    dfb.cr.adj[,11] < abs(.005))

#run coxph and crint results : after subsetting there
#weren't enough cases/contrasts to run model
cr.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                       design = comp.svy12)
summary(cr.adj.1)
table(comp.svy12$variables$CRN, comp.svy12$variables$cancMort)
#convered, but only 11 cases died with CRN
####################################################################
#Assumption 3: Log linearity with continuous predictors
####################################################################
resMart <- residuals(fitCPH, type="martingale")
plot(dfSurv$X, resMart, main="Martingale-residuals for X",
     xlab="X", ylab="Residuen", pch=20)
lines(loess.smooth(dfSurv$X, resMart), lwd=2, col="blue")
legend(x="bottomleft", col="blue", lwd=2, legend="LOESS fit", cex=1.4)



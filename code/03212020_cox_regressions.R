##########################################################################
#Name: Sarah Van Alsten
#Date Created: March 21, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, survey, survival, survminer
#Purpose: Run design-weighted Cox PH regressions to estimate associations
#between CRN and cancer-specific mortality
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

#Instructions: please get data/appropriate survey design first
#by running 01192020_NHIS_data_management.R and (at least first 30 lines of)
#01192020_NHIS_descriptives.R
#we will be usign comp.svy for all analyses below


# Collapsing Across Cancer Types ------------------------------------------
all.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                      design = comp.svy)

summary(all.unadj)

#adjusted for confounders
all.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                           factor(SEX) + factor(insurance_new) +
                            age_new + factor(race_new)+
                            yrs_any,
                         design = comp.svy)

summary(all.adjusted)


# Breast Cancer Only ------------------------------------------------------
br.svy <- subset(comp.svy, BreastCan == 1 & SEX == 2)

br.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                      design = br.svy)

summary(br.unadj)

#adjusted for confounders
br.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                          factor(insurance_new) +
                          age_new + factor(race_new)+
                          yrs_any,
                         design = br.svy)

summary(br.adjusted)

# Prostate Cancer Only ----------------------------------------------------
#(not specifying male only bc that's inherent in the data)
pr.svy <- subset(comp.svy, ProstateCan == 1 )

pr.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                     design = pr.svy)

summary(pr.unadj)

#adjusted for confounders
pr.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                          factor(insurance_new) +
                          age_new + factor(race_new)+
                          yrs_any,
                        design = pr.svy)

summary(pr.adjusted)

# Lung Cancer Only --------------------------------------------------------

lu.svy <- subset(comp.svy, LungCan == 1)

lu.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                     design = lu.svy)

summary(lu.unadj)

#adjusted for confounders
lu.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                          factor(SEX) + factor(insurance_new) +
                          age_new + factor(race_new)+
                          yrs_any,
                        design = lu.svy)

summary(lu.adjusted)


# Lymphoma Only -----------------------------------------------------------

ly.svy <- subset(comp.svy, LymphomaCan == 1 )

ly.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                     design = ly.svy)

summary(ly.unadj)

#adjusted for confounders
ly.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                          factor(SEX) + factor(insurance_new) +
                          age_new + factor(race_new)+
                          yrs_any,
                        design = ly.svy)

summary(ly.adjusted) #note loglik converged before var 12; estimates likely imprecise


# Colorectal Only ---------------------------------------------------------

cr.svy <- subset(comp.svy, ColRectCan == 1 )

cr.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN),
                     design = cr.svy)

summary(cr.unadj)

#adjusted for confounders
cr.adjusted <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN) +
                          factor(SEX) + factor(insurance_new) +
                          age_new + factor(race_new)+
                          yrs_any,
                        design = cr.svy)

summary(cr.adjusted)



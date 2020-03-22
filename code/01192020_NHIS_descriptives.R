##########################################################################
#Name: Sarah Van Alsten
#Date Created: January 19, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, tableone, survey
#Purpose: preliminary analyses of NHIS data; descriptive stats of CRN
#in individuals with breast, prostate, lung, colorectal, or lymphoma, cancer history
#Last Update: March 21, 2020
###########################################################################
#open packages
#install.packages("survey")
#install.packages("tidyverse")
#install.packages("tableone")
library(survey)
library(tableone)
library(tidyverse)

#Please run 01192020_NHIS_data_management.R first to create dataset
#needed to run the code below.

#set up the survey design for appropriate weighting
svy <-svydesign(ids = ~PSU, strata = ~ STRATA, weights = ~new_weight,
                nest = TRUE, data = analyticData)

#use subset to only keep those with a breast, prostate,  lymphoma, lung or colorectal
#cancer diagnosis in past 10 years and are eligible for mortality followup
#need to do it this way because SEs will be wrong for survey design if we
#eliminate those individuals first BEFORE setting up survey design (see survey vignette)
can.svy <- subset(svy, (yrsBreast <=10 | yrsProst <= 10 |
                    yrsColorectal <= 10 | yrsLymp <= 10 | yrsLung <=10) & MORTELIG ==1)

#make a complete cases dataset for what we will be adjusting for
#again, do it here NOT before setting up survey to get correct Standard errors
comp.svy <- subset(can.svy, !is.na(age_new) & !is.na(fuTime) & !is.na(race_new) &
                      !is.na(SEX)& !is.na(insurance_new) &
                     !is.na(CRN) & !is.na(cancMort) & !is.na(yrs_any))


#for our continuous variables (age, BMI, fuTime), visualize distributions to ensure that
#a t-test for comparisons is appropriate

#bmi by crn
comp.svy$variables %>%
  ggplot(aes(x = BMI, group = CRN, fill = factor(CRN))) +
  geom_histogram()

#age by crn
comp.svy$variables %>%
  ggplot(aes(x = age_new, group = CRN, fill = factor(CRN))) +
  geom_histogram()

#follow up time by crn
comp.svy$variables %>%
  ggplot(aes(x = fuTime, group = CRN, fill = factor(CRN))) +
  geom_histogram()

#years since dx by crn
comp.svy$variables %>%
  ggplot(aes(x = yrs_any, group = CRN, fill = factor(CRN))) +
  geom_histogram()

#none of those variables are normally distributed, instead of M(SD)
#get median and IQR and compare between CRN and no CRN

#table of descriptives : include cancer type
#to see whether there's a diff by subtype
print(
  svyCreateTableOne(vars = c("race_new", "EduR", "SmokeR", "DEAD",
                             "cancMort", "skipMed", "delayMed", "lessMed",
                             "BarrierMedR", "BMI", "SEX", "age_new",
                             "insurance_new", "IncomeR", "BreastCan", "ProstateCan",
                             "LungCan", "LymphomaCan", "ColRectCan", "fuTime", "yrs_any"),
                    strata = "CRN", 
                    data = comp.svy,
                    factorVars = c("race_new", "EduR", "SmokeR", "DEAD",
                                   "cancMort", "skipMed", "delayMed", "lessMed",
                                   "BarrierMedR", "SEX", "insurance_new", "IncomeR",
                                   "BreastCan", "ProstateCan",
                                   "LungCan", "LymphomaCan", "ColRectCan")),
  nonnormal = c("BMI", "age_new", "fuTime", "yrs_any")
  )

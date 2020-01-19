#Name: Sarah Van Alsten
#Date Created: January 19, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, ipumsr, survey
#Purpose: Read in and clean NHIS data
#Last Update: January 19, 2020
###########################################################################
#read in the IPUMS data 
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
library(ipumsr)
library(tidyverse)
library(crunch)
ddi <- read_ipums_ddi("C:\\Users\\Owner\\OneDrive\\Documents\\Fall_2019\\Capstone\\nhis2019\\data\\nhis_00010.xml")
data <- read_ipums_micro(ddi)
names(data)

subData <- data
#how many observations by wave
subData %>%
  group_by(YEAR)%>%
  summarise(n = n())

#total people who died from something including diabetes
subData %>%
  group_by(YEAR)%>%
  group_by(MORTDIAB)%>%
  summarise(n = n())

#total people who died from something including hypertension
subData %>%
  group_by(YEAR)%>%
  group_by(MORTHYPR)%>%
  summarise(n = n())

#total people eligible to have mortality followup
subData %>%
  group_by(YEAR)%>%
  group_by(MORTELIG)%>%
  summarise(n = n())

options(haven.show_pillar_labels = FALSE)
options(ipumsr.show_pillar_labels = FALSE)

#only include the people older than 18
subData<- as.data.frame(subData)
subData[] <- lapply(subData, unclass)

rm(data)
rm(ddi)
#eligible <- subData[as.integer(subData$MORTELIG) !=2,] #1= eligible, 2 = under 18, 3= ineligible, will have to weight for eligibility
eligible <- subData

#diabetes mortality
eligible %>%
  group_by(YEAR)%>%
  group_by(MORTDIAB)%>%
  summarise(n = n()) #1 = no, 2 = yes, 9 = niu

#cancer by type and by year
cancer_Type_Year <- eligible %>%
  group_by(YEAR)%>%
  summarise(Breast = sum(CNBRES==2),
            Uterine = sum(CNUTER==2),
            Thyroid = sum(CNTHYR==2),
            ThroatPharynx = sum(CNTHRO==2),
            Testicular = sum(CNTEST==2),
            Stomach = sum(CNSTOM==2),
            SoftTissue = sum(CNSOFT==2),
            SkinUnknown = sum(CNSKDK==2),
            SkinNonMelanoma = sum(CNSKNM==2),
            SkinMelanoma = sum(CNMELN==2),
            Rectal = sum(CNRECT==2),
            Colon =sum(CNCOLN==2),
            Kidney = sum(CNKIDN==2),
            Prostate = sum(CNPROS==2),
            Gallbladder = sum(CNGALL==2),
            Pancreas = sum(CNPANC==2),
            Ovarian = sum(CNOVAR==2),
            MouthLipTongue = sum(CNMOTH==2),
            Lymphoma = sum(CNLYMP==2),
            Lung = sum(CNLUNG==2),
            Leukemia = sum(CNLEUK==2),
            Liver =sum(CNLIVR==2),
            Larynx = sum(CNLARX==2),
            Esophagus = sum(CNESOP==2),
            Cervix = sum(CNCERV==2),
            Bone = sum(CNBONE==2),
            Brain = sum(CNBRAN==2),
            Blood = sum(CNBLOD==2),
            Bladder = sum(CNBLAD==2),
            Other = sum(CNOTHR==2),
            TotalPeople = n())
cancer_Type_Year$TotalCancer <- rowSums(cancer_Type_Year[,1:31])

eligible <- eligible %>%
  mutate(DiabetesRec = ifelse(DIABETICEV == 2, 1,
                              ifelse(DIABETICEV %in% c(0,7,8,9), NA, 0))) #Recode Diabetes Ever to Binary Variable
eligible <- eligible %>%
  mutate(DiabetesRec2 = ifelse(DIABETICEV %in% c(2,3), 1,
                               ifelse(DIABETICEV %in% c(0,7,8,9), NA, 0))) #Recode Diabetes Ever to Binary Variable, where borderline = TRUE
#current asthma
eligible <- eligible %>%
  mutate(Asthma = ifelse(ASTHMASTIL ==2, 1,
                         ifelse(ASTHMASTIL %in% c(0,7,8,9), NA, 0))) #Recode Diabetes Ever to Binary Variable, where borderline = TRUE

#ever had heart condition/dz
eligible <- eligible %>% 
  mutate(HeartDz = ifelse(HEARTCONEV ==2, 1,
                          ifelse(HEARTCONEV %in% c(0,7,8,9), NA, 0)))

#table(eligible$HeartDz, eligible$HEARTCONEV)

#ever had heart condition/dz
eligible <- eligible %>% 
  mutate(CHD = ifelse(CHEARTDIEV ==2, 1,
                      ifelse(CHEARTDIEV %in% c(0,7,8,9), NA, 0)))

#hypertenison
eligible <- eligible %>%
  mutate(HyperTen = ifelse(HYPERTENEV ==2, 1,
                           ifelse(HYPERTENEV %in% c(0,7,8,9), NA, 0)))

#heart attack
eligible <- eligible %>%
  mutate(HeartAtt = ifelse(HEARTATTEV ==2, 1,
                           ifelse(HEARTATTEV %in% c(0,7,8,9), NA, 0)))

#cystic fibrosis
eligible <- eligible %>%
  mutate(CysFib = ifelse(CYSTICFIEV ==2, 1,
                         ifelse(CYSTICFIEV %in% c(0,7,8,9), NA, 0)))

#hepatitis
eligible <- eligible %>%
  mutate(Hep = ifelse(HEPATEV ==2, 1,
                      ifelse(HEPATEV %in% c(0,7,8,9), NA, 0)))

#liver condition (yr)
eligible <- eligible %>%
  mutate(LivCon = ifelse(LIVERCONYR ==2, 1,
                         ifelse(LIVERCONYR %in% c(0,7,8,9), NA, 0)))

#Liver Chronic
eligible <- eligible %>%
  mutate(ChLiv= ifelse(LIVERCHRON ==2, 1,
                       ifelse(LIVERCHRON %in% c(0,7,8,9), NA, 0)))

#stroke (yr)
eligible <- eligible %>%
  mutate(Stroke= ifelse(STROKEYR ==2, 1,
                        ifelse(STROKEYR %in% c(0,7,8,9), NA, 0)))

#Stroke Ever
eligible <- eligible %>%
  mutate(StrokeYr= ifelse(STROKEYR ==2, 1,
                          ifelse(STROKEYR %in% c(0,7,8,9), NA, 0)))

#Stroke Ever
eligible <- eligible %>%
  mutate(Stroke= ifelse(STROKEV ==2, 1,
                        ifelse(STROKEV %in% c(0,7,8,9), NA, 0)))

#Angina Pectoris
eligible <- eligible %>%
  mutate(AngPec= ifelse(ANGIPECEV ==2, 1,
                        ifelse(ANGIPECEV %in% c(0,7,8,9), NA, 0)))

#mortality status: 1 = dead, 2= alive, 9 = ineligible
table(eligible$MORTSTAT)
eligible$DEAD <- ifelse(eligible$MORTSTAT == 1, 1, 
                        ifelse(eligible$MORTSTAT == 2, 0, NA))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(HeartDz==1, na.rm = T),
            Dead = sum(HeartDz==1&DEAD==1, na.rm=T),
            NoHD =sum(HeartDz==0, na.rm = T),
            NoHDDead = sum(HeartDz==0 &DEAD==1, na.rm = T))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(DiabetesRec==1, na.rm = T),
            Dead = sum(DiabetesRec==1&DEAD==1, na.rm=T),
            NoDM =sum(DiabetesRec==0, na.rm = T),
            NoDMDead = sum(DiabetesRec==0 &DEAD==1, na.rm = T))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(HyperTen==1, na.rm = T),
            Dead = sum(HyperTen==1&DEAD==1, na.rm=T),
            NoDM =sum(HyperTen==0, na.rm = T),
            NoDMDead = sum(HyperTen==0 &DEAD==1, na.rm = T))


eligible %>%
  group_by(YEAR)%>%
  summarise(sum(Asthma==1, na.rm = T),
            Dead = sum(Asthma==1&DEAD==1, na.rm=T),
            NoAs =sum(Asthma==0, na.rm = T),
            NoAsDead = sum(Asthma==0 &DEAD==1, na.rm = T))


eligible %>%
  group_by(YEAR)%>%
  summarise(sum(CHD==1, na.rm = T),
            Dead = sum(CHD==1&DEAD==1, na.rm=T),
            NoCHD =sum(CHD==0, na.rm = T),
            NoCHDDead = sum(CHD==0 &DEAD==1, na.rm = T))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(HeartAtt==1, na.rm = T),
            Dead = sum(HeartAtt==1&DEAD==1, na.rm=T),
            NoHA =sum(HeartAtt==0, na.rm = T),
            NoHADead = sum(HeartAtt==0 &DEAD==1, na.rm = T))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(AngPec==1, na.rm = T),
            Dead = sum(AngPec==1&DEAD==1, na.rm=T),
            NoHA =sum(AngPec==0, na.rm = T),
            NoHADead = sum(AngPec==0 &DEAD==1, na.rm = T))

eligible %>%
  group_by(YEAR)%>%
  summarise(sum(Stroke==1, na.rm = T),
            Dead = sum(Stroke==1&DEAD==1, na.rm=T),
            NoHA =sum(Stroke==0, na.rm = T),
            NoHADead = sum(Stroke==0 &DEAD==1, na.rm = T))

table(eligible$Stroke, eligible$DEAD)
table(eligible$AngPec, eligible$DEAD)
table(eligible$HeartAtt, eligible$DEAD)
table(eligible$HeartDz, eligible$DEAD)
table(eligible$HyperTen, eligible$DEAD)
table(eligible$CHD, eligible$DEAD)
table(eligible$HyperTen)
table(eligible$CHD)
#cancerPrevalence = cancer_Type_Year$TotalCancer/cancer_Type_Year$TotalPeople


#leading cause of death: 1= heart dz, 2=cancer/neoplasm, 3=chronic lower respiratory, 4=accident, 5=cerebrovascular dz
#6 = alzheimers, 7 = diabetes, 8 =influenza, 9=nephritis, 10=all other, 96 = NIU/NA
table(eligible$MORTUCODLD)

#binary causes of death
eligible <- eligible %>%
  mutate(CancerDeath = if_else(MORTUCODLD == 2, 1, 
                               ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(DiabDeath = if_else(MORTUCODLD == 7, 1,
                             ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(CvdDeath = if_else(MORTUCODLD == 1, 1,
                            ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(StrokeDeath = if_else(MORTUCODLD == 5, 1,
                               ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(AlzDeath = if_else(MORTUCODLD == 6, 1,
                            ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(AccDeath = if_else(MORTUCODLD == 4, 1,
                            ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(RespDeath = if_else(MORTUCODLD == 3, 1,
                             ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(FluDeath = if_else(MORTUCODLD == 8, 1,
                            ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(KidneyDeath = if_else(MORTUCODLD == 9, 1,
                               ifelse(MORTUCODLD != 96, 0, NA))) %>%
  mutate(OtherDeath = if_else(MORTUCODLD ==10, 1,
                              ifelse(MORTUCODLD != 96, 0, NA)))


#Ordinal recoding of income to Poverty Level and Binary if in Poverty
#table(eligible$POVIMP5, eligible$PovertyRec)
eligible <- eligible %>%
  mutate(PovertyRec = ifelse(POVIMP5 > 14, NA,POVIMP5)) %>%
  mutate(PovertyBinaryY = ifelse(POVIMP5 < 4, 1, 0))

#recode BMI category
eligible <- eligible %>% 
  mutate(BMI = ifelse(BMICALC %in% c(0,996), NA, BMICALC))

#create a binary cancer ever variable
eligible <- eligible %>%
  mutate(CancerEvBin = ifelse(CANCEREV ==2, 1,
                              ifelse(CANCEREV == 1, 0, NA)))

ggplot(eligible, aes(x = BMI))+ geom_density()

#recode delayed medical care due to cost, and other reasons
eligible <- eligible %>%
  mutate(DELAYCOSTR = ifelse(DELAYCOST > 2 | DELAYCOST == 0, NA, 
                             ifelse(DELAYCOST == 1, 0, 1))) %>% #care cost too much
  mutate(DELAYAPPTR = ifelse(DELAYAPPT > 2 | DELAYAPPT == 0, NA, 
                             ifelse(DELAYAPPT == 1, 0, 1))) %>% #couldn't get appt soon enough
  mutate(DELAYHRSR = ifelse(DELAYHRS > 2 | DELAYHRS == 0, NA, 
                            ifelse(DELAYHRS == 1, 0, 1))) %>% #office hours didn't work
  mutate(DELAYPHONER = ifelse(DELAYPHONE > 2 | DELAYPHONE == 0, NA, 
                              ifelse(DELAYPHONE == 1, 0, 1))) %>% #couldn't reach by phone
  mutate(DELAYTRANSR = ifelse(DELAYTRANS > 2 | DELAYTRANS == 0, NA, 
                              ifelse(DELAYTRANS == 1, 0, 1))) %>% #couldn't get transportation
  mutate(DELAYWAITR = ifelse(DELAYWAIT > 2 | DELAYWAIT == 0, NA, 
                             ifelse(DELAYWAIT == 1, 0, 1))) %>% #wait time too long
  mutate(BarrierCareR = ifelse(YBARCARE > 2 | YBARCARE == 0, NA, 
                               ifelse(YBARCARE == 1, 0, 1))) %>% #needed but couldn't afford med care
  mutate(BarrierMedR = ifelse(YBARMEDS > 2 | YBARMEDS == 0, NA, 
                              ifelse(YBARMEDS == 1, 0, 1)))%>%  #needed but couldn't afford medication
  mutate(BarrierFUR = ifelse(YBARFOLLOW > 2 | YBARFOLLOW == 0, NA, 
                             ifelse(YBARFOLLOW == 1, 0, 1)))%>%  #needed but couldn't afford followup
  mutate(BarrierSpecR = ifelse(YBARSPECL > 2 | YBARSPECL == 0, NA, 
                               ifelse(YBARSPECL == 1, 0, 1)))%>%  #needed but couldn't afford specialist
  mutate(BarrierMHR = ifelse(YBARMENTAL > 2 | YBARMENTAL == 0, NA, 
                             ifelse(YBARMENTAL == 1, 0, 1))) #needed but couldn't afford mental health care

#barriers to care by poverty
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELARYTRANSR","DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "PovertyBinaryY",
               data = eligible,
               includeNA = TRUE)


#barriers to care by diabetes status
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELARYTRANSR","DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR", "DEAD"),
               strata = "DiabetesRec",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELARYTRANSR","DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "DiabetesRec",
               data = eligible,
               includeNA = TRUE)

#barriers to care by cancer status
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELARYTRANSR","DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "CancerEvBin",
               data = eligible,
               includeNA = TRUE)

#table(eligible$CancerEvBin, eligible$DiabetesRec2)
#odds ratio of having cancer if diabetic
#(6506*378097)/(38532*29668)

#including the borderline diabetics
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "DiabetesRec2",
               data = eligible,
               includeNA = TRUE)
#everybody
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               data = eligible,
               includeNA = TRUE)

#barriers to care by cvd status
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "HeartDz",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "HeartDz",
               data = eligible,
               includeNA = F)

#barriers to care by hypertension status
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR"),
               strata = "HyperTen",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "HyperTen",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "CHD",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "HeartAtt",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYPHONER", "DELAYAPPTR",
                        "DELAYWAITR", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "AngPec",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("DELAYCOSTR", "DELAYHRSR", "DELAYAPPTR",
                        "DELAYWAITR", "DELAYPHONER", "BarrierCareR", "BarrierMedR", "BarrierFUR",
                        "BarrierSpecR", "BarrierMHR","DEAD"),
               strata = "Stroke",
               data = eligible,
               includeNA = F)
###############################################
#subset to include just 2010-2014
eligible1014 <- subset(eligible, YEAR >=2010)
CreateCatTable(vars = c("DEAD"),
               strata = "HeartDz",
               data = eligible1014,
               includeNA = F)
CreateCatTable(vars = c("DEAD"),
               strata = "DiabetesRec",
               data = eligible1014,
               includeNA = F)
#################################################
#Behaviors to Save Money on Meds
eligible <- eligible %>%
  mutate(skipMed = ifelse(YSKIPMEDYR > 2 | YSKIPMEDYR == 0, NA,
                          ifelse(YSKIPMEDYR == 1, 0,
                                 ifelse(YSKIPMEDYR ==2, 1,NA))))%>%
  mutate(delayMed = ifelse(YDELAYMEDYR > 2| YDELAYMEDYR == 0, NA,
                           ifelse(YDELAYMEDYR == 1, 0,
                                  ifelse(YDELAYMEDYR ==2, 1,NA))))%>%
  mutate(CheapMed = ifelse(YCHEAPMEDYR > 2 | YCHEAPMEDYR == 0, NA,
                           ifelse(YCHEAPMEDYR == 1, 0,
                                  ifelse(YCHEAPMEDYR ==2, 1,NA))))%>%
  mutate(foreignMed = ifelse(YFORNMEDYR > 2 | YFORNMEDYR == 0, NA,
                             ifelse(YFORNMEDYR == 1, 0,
                                    ifelse(YFORNMEDYR ==2, 1,NA))))%>%
  mutate(alternateMed = ifelse(YALTMEDYR > 2 | YALTMEDYR == 0, NA,
                               ifelse(YALTMEDYR == 1, 0,
                                      ifelse(YALTMEDYR ==2, 1,NA))))%>%
  mutate(lessMed = ifelse(YSKIMPMEDYR > 2 | YSKIMPMEDYR == 0, NA,
                          ifelse(YSKIMPMEDYR == 1, 0,
                                 ifelse(YSKIMPMEDYR ==2, 1,NA))))

#create a CRN measure
#yes if ybarmedr is as yes or any of the the 3 specific measures are a yes
eligible <- eligible %>%
  mutate(CRN = ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 
                      ifelse(is.na(BarrierMedR) & is.na(skipMed) &is.na(lessMed) &is.na(delayMed), NA, 0)))

table(eligible$CRN2, eligible$CRN, useNA = "ifany")

CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "PovertyBinaryY",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "PovertyBinaryY",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "DiabetesRec",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "DiabetesRec",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "DiabetesRec2",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "DiabetesRec2",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "HeartDz",
               data = eligible,
               includeNA = TRUE)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "HeartDz",
               data = eligible,
               includeNA = F)

CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "CancerEvBin",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "HyperTen",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "CHD",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "HeartAtt",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "AngPec",
               data = eligible,
               includeNA = F)
CreateCatTable(vars = c("skipMed", "delayMed", "CheapMed", "foreignMed", "alternateMed"),
               strata = "Stroke",
               data = eligible,
               includeNA = F)

#Create a variable that indicates ANY med cost reducing behavior...variable applys to 2011-2014
eligible %>%
  #group_by(YEAR)%>%
  summarise(skip = sum(skipMed==1, na.rm = T),
            less = sum(lessMed == 1, na.rm = T),
            delay = sum(delayMed==1, na.rm = T),
            cheap = sum(CheapMed==1, na.rm = T),
            foreign = sum(foreignMed==1, na.rm = T),
            alternate = sum(alternateMed==1, na.rm = T))


table(eligible$skipMed, eligible$delayMed) #OR = 130
#(6861*99600)/(3882*1354)
table(eligible$skipMed, eligible$CheapMed) #OR = 1.34
#(5937*8658)/(16888*2273)
table(eligible$skipMed, eligible$foreignMed) #OR = 6.06
#(668*101989)/(1489*7547)
table(eligible$skipMed, eligible$alternateMed)#OR = 10.47
#(2116*100146)/(3321*6095)
table(eligible$CheapMed, eligible$foreignMed)#OR = 4.51
#(1136*87826)/(21690*1020)
table(eligible$CheapMed, eligible$delayMed)# OR = 15.04
#(7782*85886)/(15044*2954)

#create variable for any cost related barrier to care
#DELAYCOSTR BarrierCareR BarrierMedR BarrierFUR BarrierSpecR (BarrierMHR)
eligible %>%
  group_by(YEAR)%>%
  summarise(delayCost = sum(DELAYCOSTR ==1, na.rm =T),
            NoAffordCare = sum(BarrierCareR==1, na.rm =T),
            NoAffordMed = sum(BarrierMedR==1, na.rm = T),
            NoAffordFU = sum(BarrierFUR ==1, na.rm = T),
            NoAffordSpec = sum(BarrierSpecR==1, na.rm = T),
            NoAffordMH = sum(BarrierMHR == 1, na.rm = T))

eligible %>%
  group_by(DiabetesRec)%>%
  summarise(delayCost = sum(DELAYCOSTR ==1, na.rm =T),
            NoAffordCare = sum(BarrierCareR==1, na.rm =T),
            NoAffordMed = sum(BarrierMedR==1, na.rm = T),
            NoAffordFU = sum(BarrierFUR ==1, na.rm = T),
            NoAffordSpec = sum(BarrierSpecR==1, na.rm = T),
            NoAffordMH = sum(BarrierMHR == 1, na.rm = T))


#table of med and cost barriers by cancer ever
CreateCatTable(vars = c("anyCostBarrier", "anyMedBx", "numCostBarrier", "numMedBx", "anyCostBarrierNoMH", "numCostBarrierNoMH"),
               strata = "CancerEvBin",
               data = eligible,
               includeNA = F)
#table of med and cost barriers by diabetes
CreateCatTable(vars = c("anyCostBarrier", "anyMedBx", "numCostBarrier", "numMedBx", "anyCostBarrierNoMH", "numCostBarrierNoMH"),
               strata = "DiabetesRec",
               data = eligible,
               includeNA = F)

#table of med and cost barriers by mortality status
CreateCatTable(vars = c("anyCostBarrier", "anyMedBx", "numCostBarrier", "numMedBx", "anyCostBarrierNoMH", "numCostBarrierNoMH"),
               strata = "DEAD",
               data = eligible,
               includeNA = F)

#define diabetes death only including those with diabetes
eligible <- eligible %>%
  mutate(DiabDeath2 = ifelse(DiabetesRec %in% c(0,NA), NA, DiabDeath))

#table of med and cost barriers by mortality status
CreateCatTable(vars = c("anyCostBarrier", "anyMedBx", "numCostBarrier", "numMedBx", "anyCostBarrierNoMH", "numCostBarrierNoMH"),
               strata = "DiabDeath2",
               data = eligible,
               includeNA = F)

eligible <- eligible %>%
  mutate(CancerDeath2 = ifelse(CancerEvBin %in% c(0,NA), NA, CancerDeath))

#table of med and cost barriers by mortality status
CreateCatTable(vars = c("anyCostBarrier", "anyMedBx", "numCostBarrier", "numMedBx", "anyCostBarrierNoMH", "numCostBarrierNoMH"),
               strata = "CancerDeath2",
               data = eligible,
               includeNA = F)
##################################
#Worried about health care cost: is reverse coded
eligible <- eligible %>%
  mutate(WorryHC = ifelse(WRYHCCST %in% c(0,7,8,9), NA, 4-WRYHCCST))
table(eligible$WorryHC)

eligible %>% 
  group_by(DiabetesRec)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(HeartDz)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(HyperTen)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(CHD)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(HeartAtt)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(AngPec)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

eligible %>% 
  group_by(Stroke)%>%
  summarise(worry3 = sum(WorryHC==3, na.rm =T),
            worry2 = sum(WorryHC==2, na.rm =T),
            worry1 = sum(WorryHC==1, na.rm =T),
            worry0 = sum(WorryHC==0, na.rm =T),
            mean = mean(WorryHC, na.rm=T),
            sd = sd(WorryHC, na.rm =T))

CreateCatTable(vars = c("WorryHC"),
               strata = "HeartDz",
               data = eligible1014,
               includeNA = F)
CreateCatTable(vars = c("WorryHC"),
               strata = "DiabetesRec",
               data = eligible1014,
               includeNA = F)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "HeartDz",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "DiabetesRec",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "HyperTen",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "CHD",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "HeartAtt",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "AngPec",
               data = eligible)
CreateTableOne(vars = c("BMI", "AGE", "SEX", "REGION"),factorVars = c("SEX", "REGION"),
               strata = "Stroke",
               data = eligible)
#############################################
#RACE
eligible <- eligible %>%
  mutate(RaceR = ifelse(HISPYN==2, 3, #Hispanic
                        ifelse(RACESR==100, 1, #white
                               ifelse(RACESR==200, 2, #black
                                      ifelse(RACESR<400, 4, #AI/AN
                                             ifelse(RACESR<500, 5, #Asian
                                                    ifelse(RACESR<900,6, NA))))))) #other, unknown

table(eligible$RaceR)
CreateCatTable(vars = "RaceR", strata= "DiabetesRec", data = eligible)
CreateCatTable(vars = "RaceR", strata= "HeartDz", data = eligible)
CreateCatTable(vars = "RaceR", strata= "HyperTen", data = eligible)
CreateCatTable(vars = "RaceR", strata= "CHD", data = eligible)
CreateCatTable(vars = "RaceR", strata= "HeartAtt", data = eligible)
CreateCatTable(vars = "RaceR", strata= "AngPec", data = eligible)
CreateCatTable(vars = "RaceR", strata= "Stroke", data = eligible)
###############################################
#Education level
eligible <- 
  eligible %>% 
  mutate(EduR = ifelse(EDUCREC1 == 0, NA,
                       ifelse(EDUCREC1 <= 13, 1, #HS or less
                              ifelse(EDUCREC1 < 15, 2, #some college
                                     ifelse(EDUCREC1<17,3, NA))))) #college or more
CreateCatTable(vars = "EduR", strata= "DiabetesRec", data = eligible)
CreateCatTable(vars = "EduR", strata= "HeartDz", data = eligible)
CreateCatTable(vars = "EduR", strata= "HyperTen", data = eligible)
CreateCatTable(vars = "EduR", strata= "CHD", data = eligible)
CreateCatTable(vars = "EduR", strata= "HeartAtt", data = eligible)
CreateCatTable(vars = "EduR", strata= "AngPec", data = eligible)
CreateCatTable(vars = "EduR", strata= "Stroke", data = eligible)
################################################
#Income
eligible <-
  eligible %>%
  mutate(IncomeR = ifelse(INCIMP1<5, 0,
                          ifelse(INCIMP1<20,1,
                                 ifelse(INCIMP1<42,2,
                                        ifelse(INCIMP1<62,3,
                                               ifelse(INCIMP1<66,4,
                                                      ifelse(is.na(INCIMP1),NA,5)))))))

CreateCatTable(vars = "IncomeR", strata= "DiabetesRec", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "HeartDz", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "HyperTen", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "CHD", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "HeartAtt", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "AngPec", data = eligible)
CreateCatTable(vars = "IncomeR", strata= "Stroke", data = eligible)
#####################################################
#Satisfaction With Health Care
eligible <-
  eligible%>%
  mutate(SatisHC = ifelse(HCSATIS12M>0 & HCSATIS12M<5, 5-HCSATIS12M,
                          ifelse(HCSATIS12M==5, 0, NA)),
         SatisHC2 = ifelse(HCSATIS12M>0 & HCSATIS12M<5, 5-HCSATIS12M,NA))

eligible %>%
  group_by(YEAR)%>%
  summarise(mean(SatisHC, na.rm = T))

CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "DiabetesRec", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "HeartDz", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "HyperTen", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "CHD", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "HeartAtt", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "AngPec", data = eligible)
CreateCatTable(vars = c("SatisHC","SatisHC2"), strata= "Stroke", data = eligible)
######################################################
#additional questions about paying for Health Care
eligible <-
  eligible %>%
  mutate(PayOvTime = ifelse(HIPAYMEDBIL==1,0,
                            ifelse(HIPAYMEDBIL==2,1, NA)),
         ProbPayMedBill = ifelse(HIPROBPAYR==1,0,
                                 ifelse(HIPROBPAYR==2,1, NA)),
         UnablePayMedBill = ifelse(HIUNABLEPAY==1,0,
                                   ifelse(HIUNABLEPAY==2,1, NA)),
         InsType = ifelse(HINOTCOV==2,0, #none
                          ifelse(HIPUBCOV==2,1, #public: medicaid/chip
                                 ifelse(HIPRIVATE==2,2,#private
                                        ifelse(HIMILANY ==2,3, #military
                                               ifelse(HIMCARE==2,4, #medicare
                                                      ifelse(HINONE ==1,5,NA)))))))#other

CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "DiabetesRec", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "HeartDz", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "HyperTen", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "CHD", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "HeartAtt", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "AngPec", data = eligible)
CreateCatTable(vars = c("PayOvTime",'ProbPayMedBill',"UnablePayMedBill", "InsType"),
               strata= "Stroke", data = eligible)

eligible %>%
  group_by(YEAR)%>%
  summarise(payov = sum(PayOvTime==1, na.rm = T),
            probpay = sum(ProbPayMedBill==1, na.rm =T),
            unablepay = sum(UnablePayMedBill==1,na.rm = T),
            InsType = sum(InsType==1, na.rm = T))

#############################################
eligible <- eligible %>%
  mutate(WorrySerIll = ifelse(WRYMEDCST>0 & WRYMEDCST<7, 4- WRYMEDCST, NA))

CreateCatTable(vars = "WorrySerIll", strata = "DiabetesRec", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "HeartDz", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "HyperTen", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "CHD", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "HeartAtt", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "AngPec", data = eligible)
CreateCatTable(vars = "WorrySerIll", strata = "Stroke", data = eligible)

eligible %>%
  group_by(DiabetesRec)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))

eligible %>%
  group_by(HeartDz)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))
eligible %>%
  group_by(HyperTen)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))
eligible %>%
  group_by(CHD)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))
eligible %>%
  group_by(HeartAtt)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))
eligible %>%
  group_by(AngPec)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))
eligible %>%
  group_by(Stroke)%>%
  summarise(med = mean(WorrySerIll, na.rm =T),
            IQR = sd(WorrySerIll, na.rm =T))

kruskal.test(eligible$WorrySerIll~eligible$DiabetesRec)
kruskal.test(eligible$WorrySerIll~eligible$HeartDz)
kruskal.test(eligible$WorrySerIll~eligible$HyperTen)
kruskal.test(eligible$WorrySerIll~eligible$CHD)
kruskal.test(eligible$WorrySerIll~eligible$HeartAtt)
kruskal.test(eligible$WorrySerIll~eligible$AngPec)
kruskal.test(eligible$WorrySerIll~eligible$Stroke)
################################################
#Kessler 6 Distress Scale
eligible <- eligible %>%
  mutate(ASADR = ifelse(ASAD<6, ASAD, NA),
         AEFFORTR = ifelse(AEFFORT<6, AEFFORT, NA),
         ARESTLESSR = ifelse(ARESTLESS <6, ARESTLESS, NA),
         AHOPELESSR = ifelse(AHOPELESS<6, AHOPELESS, NA),
         ANERVOUSR = ifelse(ANERVOUS<6, ANERVOUS, NA),
         AWORTHLESSR = ifelse(AWORTHLESS<6, AWORTHLESS, NA))

eligible <- eligible %>%
  mutate(Kessler6 = (ASADR+ AEFFORTR+ ARESTLESSR+ AHOPELESSR+ ANERVOUSR+ AWORTHLESSR))
eligible$Kessler6Bin <- ifelse(eligible$Kessler6 >=13, 1,0)

eligible%>%
  group_by(DiabetesRec)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())
eligible%>%
  group_by(HeartDz)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())

eligible%>%
  group_by(HyperTen)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())

eligible%>%
  group_by(CHD)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())

eligible%>%
  group_by(HeartAtt)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())

eligible%>%
  group_by(AngPec)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())

eligible%>%
  group_by(Stroke)%>%
  summarise(MedKess = median(Kessler6, na.rm = T),
            IQRKess = IQR(Kessler6, na.rm =T),
            Kess13 = sum(Kessler6Bin==1, na.rm =T),
            n())


kruskal.test(eligible$Kessler6~eligible$DiabetesRec)
kruskal.test(eligible$Kessler6~eligible$HeartDz)
kruskal.test(eligible$Kessler6~eligible$HyperTen)
kruskal.test(eligible$Kessler6~eligible$CHD)
#####################################################3
#Smoking 
eligible <-
  eligible %>%
  mutate(SmokeR = ifelse(SMOKESTATUS2 %in% c(10,11,12,13), 2, #current
                         ifelse(SMOKESTATUS2 %in% c(20,40),1, #former
                                ifelse(SMOKESTATUS2 ==30, 0, NA)))) #never
CreateCatTable(vars = "SmokeR", strata = "DiabetesRec", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "HeartDz", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "HyperTen", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "CHD", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "HeartAtt", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "AngPec", data = eligible)
CreateCatTable(vars = "SmokeR", strata = "Stroke", data = eligible)
########################################################
table(eligible$DiabetesRec)
table(eligible$DIABETICEV)
table(eligible$DEAD)
table(eligible$DEAD, eligible$DiabetesRec)
table(eligible$DELAYCOSTR)
table(eligible$CNBRES)
table(eligible$CNUTER)
table(eligible$CNCERV)
table(eligible$CNBRES, eligible$DEAD)

eligible %>%
  group_by(YEAR)%>%
  summarise(DiabDied = sum(DiabetesRec ==1&DEAD==1, na.rm = T),
            DiabDiedDM = sum(DiabetesRec==1 & MORTDIAB==1, na.rm = T),
            NoDMDead =  sum(DiabetesRec==0 & MORTDIAB==1, na.rm = T))


eligible$diabNIU <- ifelse(eligible$DIABETICEV==0, 1,NA)
table(eligible$diabNIU, eligible$YEAR)
table(eligible$DiabetesRec, eligible$YEAR)

##########################################################################################
#generate follow up times
#table(eligible$MORTDODY)
#table(eligible$MORTDODQ) #1=Jan thru March, 2=April-June, 3=July-Sep, 4=Oct-Dec
#End of quarter: March 31, June 30, September 30, December 31 aka day 90, day 180, day 270, day 365
eligible <- eligible %>%
  mutate(deathDayOfYear = ifelse(MORTDODQ == 1, 90,
                                 ifelse(MORTDODQ == 2, 180,
                                        ifelse(MORTDODQ == 3, 270,
                                               ifelse(MORTDODQ ==4, 365, NA)))))

#exclude those where we don't know year they were interviewed in: 12 people
eligible <- eligible[eligible$INTERVWYR<=2015,]

#use interview month and year to determine START of FU time
eligible$leapYear <- ifelse(eligible$INTERVWYR %in% c(2000,2004,2008,2012),1,0)

#day of year
eligible <- eligible %>%
  mutate(dayOfYear = ifelse(INTERVWMO==1,1,
                            ifelse(INTERVWMO==2,32,
                                   ifelse(INTERVWMO==3,60,
                                          ifelse(INTERVWMO==4,91,
                                                 ifelse(INTERVWMO==5,121,
                                                        ifelse(INTERVWMO==6,152,
                                                               ifelse(INTERVWMO==7,182,
                                                                      ifelse(INTERVWMO==8,212,
                                                                             ifelse(INTERVWMO==9,243,
                                                                                    ifelse(INTERVWMO==10,273,
                                                                                           ifelse(INTERVWMO==11,304,
                                                                                                  ifelse(INTERVWMO==12,334,NA)))))))))))))
eligible <-
  eligible %>%
  mutate(dayOfYear = ifelse(leapYear==1 & INTERVWMO>2, dayOfYear+1, dayOfYear)) #leap year add a day

#table(eligible$dayOfYear)
#now set an initial starting date to which these will be added
startDate <- as.Date("2000-01-01")

#enrollment is round(start date + dayOfYear + 365.25*(YEAR-2000))
eligible$enrollDate <- round(startDate + eligible$dayOfYear + 365.25*(eligible$INTERVWYR -2000))
#death date is round(start date + deathDayOfYear + 365*(YEARofdeath -2000))
eligible$deathDate <- round(startDate + eligible$deathDayOfYear + 365.25*(eligible$MORTDODY-2000))

#follow-up time is either time from enrollment to death date OR time from enrollment to Dec 31, 2015
eligible <- eligible %>%
  mutate(fuTime = ifelse(DEAD == 1, 
                         difftime(deathDate, enrollDate,units = "weeks"), #if dead, go to date of death
                         difftime("2015-12-31", enrollDate, units = "weeks"))) #if alive, right censor


summary(eligible$fuTime)

#look and see those whose interview supposedly b4 death
eligible[eligible$fuTime<0,]
#table(eligible$enrollDate, useNA = "ifany")
#table(eligible$deathDate, useNA = "ifany")
#table(eligible$YEAR, useNA = "ifany")

inconFU <- eligible[eligible$fuTime < 0 & !is.na(eligible$fuTime),]
inconFU %>% summarise(n())
#write out inconsistent FU to CSV
write.csv(inconFU, "C:\\Users\\Owner\\OneDrive\\Documents\\Fall_2019\\Capstone\\nhis\\data\\inconFU.csv")

#table(eligible$MORTDODQ, eligible$MORTDODY)
#43 individuals who reportedly died in a year/quarter prior to their interview, 
#10 of whom had diabetes and 8 of whom had cancer (4 had both)
#8 had hypertension, 7 had CHD, 6 had a heart dz
nhis <- eligible[eligible$fuTime > 0 | is.na(eligible$fuTime),]
rm(subData)



table(eligible$DiabDeath,  eligible$BarrierMedR)
table(eligible$CvdDeath,  eligible$BarrierMedR)
table(eligible$MORTDIAB,  eligible$BarrierMedR)
table(eligible$MORTHYPR,  eligible$BarrierMedR)

#Add up number of conditions
eligible$AnyHC <-ifelse(eligible$CHD==1 | eligible$HeartAtt==1 | eligible$AngPec==1 |
                          eligible$HeartDz==1, 1,
                        ifelse(eligible$CHD==0 & eligible$HeartAtt==0 & eligible$AngPec==0 &
                                 eligible$HeartDz==0, 0, NA))

eligible$AnyHCHT <- ifelse(eligible$AnyHC==1 | eligible$HyperTen==1,1,
                           ifelse(eligible$AnyHC==0 & eligible$HyperTen==0, 0 , NA))

eligible$AnyCVD <- ifelse(eligible$AnyHC==1 | eligible$Stroke==1, 1,
                          ifelse(eligible$AnyHC == 0 & eligible$Stroke==0, 0, NA))

eligible$AnyCVDHT <- ifelse(eligible$AnyCVD==1 | eligible$HyperTen==1, 1,
                            ifelse(eligible$AnyCVD == 0 & eligible$HyperTen==0, 0, NA))


#above was LEADING cause of death: now do if it was listed as a cause (not necessarily leading)
table(eligible$MORTUCOD)
#046 = Diabetes, 056 = hypertensive dz, 057 = hypertensive heart, renal, 059 - 075 = cardio diagnoses
#100 = renal failure

eligible <- eligible %>%
  mutate(DzSpecificDiab = ifelse(MORTUCOD == 46 | MORTUCODLD == 7 | MORTDIAB == 2, 1,
                                 ifelse(DEAD == 1, 0 , NA))) %>%
  mutate(DzSpecificDiab_NoNA = ifelse(MORTUCOD == 46 | MORTUCODLD == 7 | MORTDIAB == 2, 1, 0)) %>%
  mutate(DzSpecificCVDHT = ifelse(MORTUCOD >=56 & MORTUCOD <= 75 | MORTHYPR == 2, 1,
                                  ifelse(MORTUCODLD == 1 | MORTUCODLD == 5, 1, ifelse(DEAD == 1, 0 ,NA)))) %>%
  mutate(DzSpecificCVDHT_NoNA = ifelse(MORTUCOD >=56 & MORTUCOD <= 75 | MORTHYPR == 2, 1,
                                       ifelse(MORTUCODLD == 1 | MORTUCODLD == 5, 1, 0))) %>%
  mutate(DzSpecificCVD = ifelse(MORTUCOD >=56 & MORTUCOD <= 75 , 1,
                                ifelse(MORTUCODLD == 1 | MORTUCODLD == 5, 1, ifelse(DEAD == 1, 0 ,NA)))) %>%
  mutate(DzSpecificCVD_NoNA = ifelse(MORTUCOD >=56 & MORTUCOD <= 75, 1,
                                     ifelse(MORTUCODLD == 1 | MORTUCODLD == 5, 1, 0)))

table(eligible$DzSpecificCVD)
table(eligible$DzSpecificCVD_NoNA)
table(eligible$DzSpecificCVDHT)
table(eligible$DzSpecificCVDHT_NoNA)
table(eligible$DzSpecificDiab)
table(eligible$DzSpecificDiab_NoNA)

table(eligible$DzSpecificDiab, eligible$DiabetesRec2, useNA = "ifany")
table(eligible$DzSpecificCVD, eligible$AnyCVD, useNA = "ifany")


eligible%>% group_by(YEAR)%>%summarise(mean(MORTDIAB, na.rm = T))

write.csv(eligible, "C:\\Users\\Owner\\OneDrive\\Documents\\Fall_2019\\Capstone\\nhis2019\\data\\eligible.csv")
eligible <- read.csv("data\\eligible.csv")

table(eligible$DzSpecificDiab, eligible$DzSpecificDiab_NoNA, useNA = "ifany")

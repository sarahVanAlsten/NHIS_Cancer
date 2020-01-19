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

#install.packages("ipumsr") #to access IPUMS data
#install.packages("tidyverse") #for data cleaning
library(ipumsr)
library(tidyverse)

#read in data
ddi <- read_ipums_ddi("data\\nhis_00010.xml")
subData <- read_ipums_micro(ddi) 

#take a look at variable names
names(subData)

#summarise how many observations by wave
subData %>%
  group_by(YEAR)%>%
  summarise(n = n())

#summarise how many observations eligible for mortality FU
#1 = eligible, 2 = under 18/ineligible, 3 = not enough info/ineligible
subData %>%
  group_by(MORTELIG) %>%
  summarise(n())

#create summary data of all individuals with cancer by year and type
#cancer by type and by year
cancer_Type_Year <- subData %>%
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
            TotalPeople = n()) #total people = total responses in that year

#get total number of people with ANY cancer by year
cancer_Type_Year$TotalCancer <- rowSums(cancer_Type_Year[,1:31])

#additionally, summarise the total # people with each type across years by summing all columns
total_cancer_type <- 
  cancer_Type_Year %>%
  summarise_all(.funs = sum)


#mortality status: 1 = dead, 2= alive, 9 = ineligible
table(subData$MORTSTAT)
subData$DEAD <- ifelse(subData$MORTSTAT == 1, 1, 
                        ifelse(subData$MORTSTAT == 2, 0, NA))


#leading cause of death:
#1= heart dz, 2=cancer/neoplasm, 3=chronic lower respiratory, 4=accident, 5=cerebrovascular dz
#6 = alzheimers, 7 = diabetes, 8 =influenza, 9=nephritis, 10=all other, 96 = NIU/NA (bc ineligible)

#binary leading cause of death recorded
#make people who didn't die or were ineligible missing for now so 0 = dead but not of that cause
subData <- subData %>%
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
  mutate(OtherDeath = if_else(MORTUCODLD == 10, 1,
                              ifelse(MORTUCODLD != 96, 0, NA)))

#above was LEADING cause of death, which is only available for deaths 2004
#now, add in the more specific and granular causes, which were available from 2000-2004
#cancers are codes between (and including) 20 - 44.
subData <- subData %>%
  mutate(cancMort = ifelse(DEAD == 0, 0, #not dead, so couldnt' die of cancer
                           ifelse(CancerDeath == 1, 1, #carry forward the leading COD
                                  ifelse(YEAR <= 2004 & MORTUCOD >= 20 & MORTUCOD <= 44, 1, #include contributors
                                         0)))) #cancer not listed as leading or contributing factor

#check codng: everyone in row w/ "1" should be in column with "1" (cancer death --> cancer mort)
#some in col "1" potentially not in row "1" --> had cancer listed as contributor but not leading
table(subData$CancerDeath, subData$cancMort, useNA = "ifany")


#Ordinal recoding of income to Poverty Level and Binary if in Poverty
#14 is highest category... everything above is missing
#Less than 4 = Below FPL
subData <- subData %>%
  mutate(PovertyRec = ifelse(POVIMP5 > 14, NA, POVIMP5)) %>%
  mutate(PovertyBinaryY = ifelse(POVIMP5 < 4, 1, 0))

#recode missings in BMI and then create categorical BMI variable
subData <- subData %>% 
  mutate(BMI = ifelse(BMICALC %in% c(0,996), NA, BMICALC),
         BMIcat = ifelse(BMI < 18.5, 1,
                         ifelse(BMI < 25, 2,
                                ifelse(BMI < 30, 3, 4)))) #missings stay missing bc can't compare NA to number

#create a binary cancer ever variable in original data
subData <- subData %>%
  mutate(CancerEvBin = ifelse(CANCEREV == 2, 1,
                              ifelse(CANCEREV == 1, 0, NA)))

#create cancer type variable in orginal data. For this, 
#1 = YES had that type, missing is DK/RF/Not ascertained, and 0 = NO (including no cancer and cancer but not that type)
subData <- subData %>%
  mutate(
    BreastCan = ifelse(CNBRES == 2, 1,
                       ifelse(CNBRES %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    UterineCan = ifelse(CNUTER == 2, 1,
                        ifelse(CNUTER %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    ThyroidCan = ifelse(CNTHYR == 2, 1,
                        ifelse(CNTHYR %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    ThroatPharynxCan = ifelse(CNTHRO == 2, 1,
                              ifelse(CNTHRO %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    TesticularCan = ifelse(CNTEST == 2, 1,
                           ifelse(CNTEST %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    StomachCan = ifelse(CNSTOM == 2, 1,
                        ifelse(CNSTOM %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    SoftTissueCan = ifelse(CNSOFT == 2, 1,
                           ifelse(CNSOFT %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    SkinUnknownCan = ifelse(CNSKDK == 2, 1,
                            ifelse(CNSKDK %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    SkinNonMelanomaCan = ifelse(CNSKNM == 2, 1,
                                ifelse(CNSKNM %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    SkinMelanomaCan = ifelse(CNMELN == 2, 1,
                             ifelse(CNMELN %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    RectalCan = ifelse(CNRECT == 2, 1,
                       ifelse(CNRECT %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    ColonCan = ifelse(CNCOLN == 2, 1,
                      ifelse(CNCOLN %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    KidneyCan = ifelse(CNKIDN == 2, 1,
                       ifelse(CNKIDN %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    ProstateCan = ifelse(CNPROS == 2, 1,
                         ifelse(CNPROS %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    GallbladderCan = ifelse(CNGALL == 2, 1,
                            ifelse(CNGALL %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    PancreasCan = ifelse(CNPANC == 2, 1,
                         ifelse(CNPANC %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    OvarianCan = ifelse(CNOVAR == 2, 1,
                        ifelse(CNOVAR %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    MouthLipTongueCan = ifelse(CNMOTH == 2, 1,
                            ifelse(CNMOTH %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    LymphomaCan = ifelse(CNLYMP == 2, 1,
                         ifelse(CNLYMP %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    LungCan = ifelse(CNLUNG == 2, 1,
                     ifelse(CNLUNG %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    LeukemiaCan = ifelse(CNLEUK == 2, 1,
                         ifelse(CNLEUK %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    LiverCan = ifelse(CNLIVR == 2, 1,
                      ifelse(CNLIVR %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    LarynxCan = ifelse(CNLARX == 2, 1,
                       ifelse(CNLARX %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    EsophagusCan = ifelse(CNESOP == 2, 1,
                          ifelse(CNESOP %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    CervixCan = ifelse(CNCERV == 2, 1,
                       ifelse(CNCERV %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    BoneCan = ifelse(CNBONE == 2, 1,
                     ifelse(CNBONE %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    BrainCan = ifelse(CNBRAN == 2, 1,
                      ifelse(CNBRAN %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    BloodCan = ifelse(CNBLOD == 2, 1,
                      ifelse(CNBLOD %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    BladderCan = ifelse(CNBLAD == 2, 1,
                        ifelse(CNBLAD %in% c(7,8,9) | is.na(CancerEvBin), NA, 0)),
    OtherCan = ifelse(CNOTHR == 2, 1,
                      ifelse(CNOTHR %in% c(7,8,9) | is.na(CancerEvBin), NA, 0))
  )

#make sure that coding was correct (is everyone with a 1 in a cancer type in cancerEv and is
#everyone with a 0/NA in a cancer type NOT in cancerEv)
check <- 
  subData %>%
  select(-CancerDeath) %>% #don't include death bc people could develop cancer + die after interview
  group_by(CancerEvBin) %>%
  summarise_at(vars(contains("Can", ignore.case = FALSE)), .funs = ~(sum(. == 1, na.rm = T)))
    

#age of cancer diagnosis for the different types
#use REGEX to select cols beginning with CN and ending with AG
#recode 0 (not in use) to missing, and anything over 85 (which is suppressed) to NA
subData <- subData %>%
  mutate_at(vars(matches("CN.*AG$", ignore.case = F)), 
            .funs = ~(ifelse(. == 0, NA, ifelse(. <= 85, ., NA))))

#recode delayed medical care due to cost, and other reasons
subData <- subData %>%
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

#Behaviors to Save Money on Meds
subData <- subData %>%
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
#only one question (BarrierMedR) asked in 2000-2010,
#in later years, asked specifics (lessMed, skipMed, delayMed and BarrierMedR)
#any yes = YES(1), if everything missing stay missing (NA), else no(0)
subData <- subData %>%
  mutate(CRN = ifelse(is.na(BarrierMedR) & YEAR <=2010, NA,
                      ifelse(is.na(BarrierMedR)& is.na(skipMed) & is.na(lessMed) &is.na(delayMed) & (YEAR >=2011), NA,
                             ifelse(BarrierMedR == 0 & YEAR <=2010, 0,
                                    ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 0)))))

##################################
#Worried about health care cost: is reverse coded
subData <- subData %>%
  mutate(WorryHC = ifelse(WRYHCCST %in% c(0,7,8,9), NA, 4-WRYHCCST))

#############################################
#RACE self reported
subData <- subData %>%
  mutate(RaceR = ifelse(HISPYN==2, 3, #Hispanic
                        ifelse(RACESR==100, 1, #white
                               ifelse(RACESR==200, 2, #black
                                      ifelse(RACESR<400, 4, #AI/AN
                                             ifelse(RACESR<500, 5, #Asian
                                                    ifelse(RACESR<900,6, NA))))))) #other, unknown

###############################################
#Education level
subData <- 
  subData %>% 
  mutate(EduR = ifelse(EDUCREC1 == 0, NA,#not in use/refused
                       ifelse(EDUCREC1 <= 13, 1, #HS or less
                              ifelse(EDUCREC1 < 15, 2, #some college
                                     ifelse(EDUCREC1<17,3, NA))))) #college or more

################################################
#Income categorical
#While categories are not same size, NHIS coding is not consistent in groupings.
#Some potential codings are 10k wide, others are 5k wide, thus the diff gap size btwn categories
subData <-
  subData %>%
  mutate(IncomeR = ifelse(INCIMP1<5, 0, #<25k
                          ifelse(INCIMP1<22,1, #<45k
                                 ifelse(INCIMP1<52,2, #<75k
                                        ifelse(is.na(INCIMP1),NA,
                                               5)))))#75k +

####################################################
#Satisfaction With Health Care
#0, 6,7,8,9 = missing, all others are reverse coded, 5= didn't get healthcare
#SatisHC = treated those without healthcare as reporting a 0, SatisHC2 treats them as missing
subData <-
  subData%>%
  mutate(SatisHC = ifelse(HCSATIS12M>0 & HCSATIS12M<5, 5-HCSATIS12M,
                          ifelse(HCSATIS12M==5, 0, NA)),
         SatisHC2 = ifelse(HCSATIS12M>0 & HCSATIS12M<5, 5-HCSATIS12M, NA))

######################################################
#additional questions about paying for Health Care
subData <-
  subData %>%
  #currently paying medical bills over time
  mutate(PayOvTime = ifelse(HIPAYMEDBIL==1,0,
                            ifelse(HIPAYMEDBIL==2,1, NA)),
         #currently have problems payign med bills
         ProbPayMedBill = ifelse(HIPROBPAYR==1,0,
                                 ifelse(HIPROBPAYR==2,1, NA)),
         #unable to pay medical bills altogether
         UnablePayMedBill = ifelse(HIUNABLEPAY==1,0,
                                   ifelse(HIUNABLEPAY==2,1, NA)),
         #type of health insurance
         InsType = ifelse(HINOTCOV==2,0, #none
                          ifelse(HIPUBCOV==2,1, #public: medicaid/chip
                                 ifelse(HIPRIVATE==2,2,#private
                                        ifelse(HIMILANY ==2,3, #military
                                               ifelse(HIMCARE==2,4, #medicare
                                                      ifelse(HINONE ==1,5,NA)))))))#other

#############################################
#degree of worry that wouldn't be able to pay for a serious illness, reverse coded
#0, 8, 9 missing
subData <- subData %>%
  mutate(WorrySerIll = ifelse(WRYMEDCST>0 & WRYMEDCST<7, 4- WRYMEDCST, NA))
################################################
#Kessler 6 Distress Scale: proxy for depression
#first recode anything >6 (RF/DK) to missing
subData <- subData %>%
  mutate(ASADR = ifelse(ASAD<6, ASAD, NA),
         AEFFORTR = ifelse(AEFFORT<6, AEFFORT, NA),
         ARESTLESSR = ifelse(ARESTLESS <6, ARESTLESS, NA),
         AHOPELESSR = ifelse(AHOPELESS<6, AHOPELESS, NA),
         ANERVOUSR = ifelse(ANERVOUS<6, ANERVOUS, NA),
         AWORTHLESSR = ifelse(AWORTHLESS<6, AWORTHLESS, NA))

#add together to get scale total
subData <- subData %>%
  mutate(Kessler6 = (ASADR+ AEFFORTR+ ARESTLESSR+ AHOPELESSR+ ANERVOUSR+ AWORTHLESSR))

#per recommendations: 13 or more = severe distress
subData$Kessler6Bin <- ifelse(subData$Kessler6 >=13, 1,0)

#####################################################3
#Smoking 
subData <-
  subData %>%
  mutate(SmokeR = ifelse(SMOKESTATUS2 %in% c(10,11,12,13), 2, #current, including all pack/day amts
                         ifelse(SMOKESTATUS2 %in% c(20,40),1, #former
                                ifelse(SMOKESTATUS2 ==30, 0, NA)))) #never

##########################################################################################
#generate follow up times
#1=Jan thru March, 2=April-June, 3=July-Sep, 4=Oct-Dec
#End of quarter: March 31, June 30, September 30, December 31 aka day 90, day 180, day 270, day 365
subData <- subData %>%
  mutate(deathDayOfYear = ifelse(MORTDODQ == 1, 90,
                                 ifelse(MORTDODQ == 2, 180,
                                        ifelse(MORTDODQ == 3, 270,
                                               ifelse(MORTDODQ ==4, 365, NA)))))

#use interview month and year to determine START of FU time
subData$leapYear <- ifelse(subData$INTERVWYR %in% c(2000,2004,2008,2012),1,0)

#day of year of interview, correspond to 1st of month bc we dont have exact day
subData <- subData %>%
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
#if its a leap year add an extra day if after February
subData <-
  subData %>%
  mutate(dayOfYear = ifelse(leapYear==1 & INTERVWMO>2, dayOfYear+1, dayOfYear)) #leap year add a day

#now set an initial starting date to which these will be added
startDate <- as.Date("2000-01-01")

#enrollment day since start is round(start date + dayOfYear + 365.25*(YEAR-2000))
subData$enrollDate <- round(startDate + subData$dayOfYear + 365.25*(subData$INTERVWYR -2000))

#death day since start is round(start date + deathDayOfYear + 365*(YEARofdeath -2000))
subData$deathDate <- round(startDate + subData$deathDayOfYear + 365.25*(subData$MORTDODY-2000))

#follow-up time is either time from enrollment to death date OR time from enrollment to Dec 31, 2015
subData <- subData %>%
  mutate(fuTime = ifelse(DEAD == 1, 
                         difftime(deathDate, enrollDate,units = "weeks"), #if dead, go to date of death
                         difftime("2015-12-31", enrollDate, units = "weeks"))) #if alive, right censor


#write out the cleaned data to reuse in future
write.csv(subData, "data\\nhis_cleaned.csv")


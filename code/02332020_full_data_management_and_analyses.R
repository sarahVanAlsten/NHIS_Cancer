##########################################################################
#Name: Sarah Van Alsten
#Date Created: March 22, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, survey, survival, survminer, tableone, ipumsr
#Purpose: Create one file that does all analyses start to finish.
#Last Update: March 22, 2020
###########################################################################
#read in the IPUMS data 
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
#warning: it's a BIG dataset and might take a while to load

#install.packages("ipumsr") #to access IPUMS data
#install.packages("tidyverse") #for data cleaning
library(ipumsr)
library(tidyverse)

#read in data
ddi <- read_ipums_ddi("data\\nhis_00010.xml")
subData <- read_ipums_micro(ddi) 

#summarise how many observations by wave
subData %>%
  group_by(YEAR)%>%
  summarise(n = n())

#summarise how many observations eligible for mortality FU
#1 = eligible, 2 = under 18/ineligible, 3 = not enough info/ineligible
subData %>%
  group_by(MORTELIG) %>%
  summarise(n())

#create summary data of all individuals with cancer by year and type to get N's
#all 2's = YES to a question "Were you ever diagnosed with ___x___ cancer?"
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

#get total number of people with ANY cancer by year (col 1:31 = cancer type columns)
cancer_Type_Year$TotalCancer <- rowSums(cancer_Type_Year[,1:31])

#additionally, summarise the total # people with each type across years by summing all columns
total_cancer_type <- 
  cancer_Type_Year %>%
  summarise_all(.funs = sum)


#mortality status: 1 = dead, 2= alive, 9 = ineligible
table(subData$MORTSTAT)

#make a binary yes/no mortality variable
subData$DEAD <- ifelse(subData$MORTSTAT == 1, 1, 
                       ifelse(subData$MORTSTAT == 2, 0, NA))


#leading cause of death:
#1= heart dz, 2=cancer/neoplasm, 3=chronic lower respiratory, 4=accident, 5=cerebrovascular dz
#6 = alzheimers, 7 = diabetes, 8 =influenza, 9=nephritis, 10=all other, 96 = NIU/NA (bc ineligible)

#binary leading cause of death recorded
#make people who didn't die or were ineligible missing for now so 0 = dead, just not of the
#specified cause, 1= dead of specified cause
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

#above was LEADING cause of death, which is only available for deaths 2004 of later
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

#recode missings in BMI and then create categorical BMI variable
#using the normal cutoffs (18.5, 25, 30 for underweight, overweight, obese)
subData <- subData %>% 
  mutate(BMI = ifelse(BMICALC %in% c(0,996), NA, BMICALC),
         BMIcat = ifelse(BMI < 18.5, 1,
                         ifelse(BMI < 25, 2,
                                ifelse(BMI < 30, 3, 4)))) #missings stay missing bc can't compare NA to number

#create a binary cancer ever variable in original data... any type of cancer
subData <- subData %>%
  mutate(CancerEvBin = ifelse(CANCEREV == 2, 1,
                              ifelse(CANCEREV == 1, 0, NA)))

#create cancer type variable in orginal data. For this, 
#1 = YES had that type, missing is DK/RF/Not ascertained, and
#0 = NO (including no cancer and cancer but not that type)
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
#View(check)
#looks good   

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
#alternate and foreign med not included bc these are substitiution NOT abstinence
#medication bx's and don't capture construct we're going for
subData <- subData %>%
  mutate(CRN = ifelse(is.na(BarrierMedR) & YEAR <=2010, NA,
                      ifelse(is.na(BarrierMedR)& is.na(skipMed) & is.na(lessMed) &is.na(delayMed) & (YEAR >=2011), NA,
                             ifelse(BarrierMedR == 0 & YEAR <=2010, 0,
                                    ifelse(BarrierMedR == 1 | skipMed == 1 | lessMed == 1 | delayMed == 1, 1, 0)))))

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
subData <- subData %>% 
  mutate(EduR = ifelse(EDUCREC1 == 0, NA,#not in use/refused
                       ifelse(EDUCREC1 <= 12, 1, #less than HS
                              ifelse(EDUCREC1 == 13, 2, #hs degree only
                                     ifelse(EDUCREC1 < 15, 3, #some college
                                            ifelse(EDUCREC1<17, 4, NA)))))) #college or more



######################################################
#additional questions about paying for Health Care
subData <- subData %>%
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


#####################################################3
#Smoking 
subData <- subData %>%
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
subData <- subData %>%
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


#how long ago since cancer diagnosis
subData <- subData %>%
  mutate(yrsBreast = AGE - CNBRESAG,
         yrsLung = AGE - CNLUNGAG,
         yrsColon = AGE - CNCOLNAG,
         yrsMelan = AGE - CNMELNAG,
         yrsNonMelan = AGE - CNSKNMAG,
         yrsOthSkin = AGE - CNSKDKAG,
         yrsUter = AGE - CNUTERAG,
         yrsBlad = AGE - CNBLADAG,
         yrsRect = AGE - CNRECTAG,
         yrsCerv = AGE - CNCERVAG,
         yrsLymp = AGE - CNLYMPAG,
         yrsLiver = AGE - CNLIVRAG,
         yrsLeuk = AGE- CNLEUKAG,
         yrsGall = AGE - CNGALLAG,
         yrsStom = AGE - CNSTOMAG,
         yrsSoft = AGE - CNSOFTAG,
         yrsEsoph = AGE - CNESOPAG,
         yrsProst = AGE - CNPROSAG,
         yrsPanc = AGE - CNPANCAG,
         yrsKidney = AGE - CNKIDNAG,
         yrsThyroid = AGE - CNTHYRAG,
         yrsTest = AGE - CNTESTAG,
         yrsBrain = AGE - CNBRANAG,
         yrsBone = AGE - CNBONEAG)


#individuals with a dx 10 years or less ago
within10 <- subData %>%
  filter(yrsBreast <= 10 | yrsLung <= 10 | yrsColon <= 10 | yrsMelan <= 10 |
           yrsNonMelan <=10 | yrsOthSkin <= 10 | yrsUter <= 10 | yrsBlad <= 10 |
           yrsRect <= 10 | yrsCerv <= 10 | yrsLymp <= 10 |
           yrsLiver <= 10 | yrsLeuk <=10 | yrsGall <=10 | yrsStom <=10 | 
           yrsEsoph <=10 | yrsProst <= 10 | yrsPanc <=10 | yrsKidney <= 10 |
           yrsThyroid <=10 | yrsTest <= 10 | yrsBrain <=10 | yrsBone <= 10)

#individuals within 5 yrs
within5 <- subData %>%
  filter(yrsBreast <= 5 | yrsLung <= 5 | yrsColon <= 5 | yrsMelan <= 5 |
           yrsNonMelan <=5 | yrsOthSkin <= 5 | yrsUter <= 5 | yrsBlad <= 5 |
           yrsRect <= 5 | yrsCerv <= 5 | yrsLymp <= 5 |
           yrsLiver <= 5 | yrsLeuk <=5 | yrsGall <=5 | yrsStom <=5 | 
           yrsEsoph <=5 | yrsProst <= 5 | yrsPanc <=5 | yrsKidney <= 5 |
           yrsThyroid <=5 | yrsTest <= 5 | yrsBrain <=5 | yrsBone <= 5)

#individuals diagnosed within 1 yr
within1 <- subData %>%
  filter(yrsBreast <= 1 | yrsLung <= 1 | yrsColon <= 1 | yrsMelan <= 1 |
           yrsNonMelan <=1 | yrsOthSkin <= 1 | yrsUter <= 1 | yrsBlad <= 1 |
           yrsRect <= 1 | yrsCerv <= 1 | yrsLymp <= 1 |
           yrsLiver <= 1 | yrsLeuk <=1 | yrsGall <=1 | yrsStom <=1 | 
           yrsEsoph <=1 | yrsProst <= 1 | yrsPanc <=1 | yrsKidney <= 1 |
           yrsThyroid <=1 | yrsTest <= 1 | yrsBrain <=1 | yrsBone <= 1)


#make a colorectal cancer ever + colorectal age
#(currently it is separated into colon and rectal individually)
#first see if any individual had both
table(subData$ColonCan, subData$RectalCan) #27 indivduals reported both

#look at discrepancy in age of diagnosis for these 14 individuals
colorectal <- 
  subData %>%
  filter(ColonCan ==1 & RectalCan == 1) %>%
  select(CNCOLNAG, CNRECTAG) 

#make one cancer column for the combination of the two
subData <- subData %>%
  rowwise()%>%
  mutate(ColRectCan = ifelse(ColonCan == 1 | RectalCan == 1, 1, 0),
         #age at dx = max of two to increase possible sample size of dx in last 5 yrs
         #or missing if both are missing
         ColRectAge = ifelse(is.na(CNCOLNAG) & is.na(CNRECTAG), NA,
                             max(CNCOLNAG, CNRECTAG, na.rm = T))) 

#create a yrs ago var for colorectal
subData <- subData %>%
  mutate(yrsColorectal = AGE - ColRectAge)

#how many cases will we have if we restrict to those with dx in past 5 yrs?
subData %>%
  ungroup() %>% #remove the rowwise designation from above
  select(yrsColorectal, yrsBreast, yrsProst, yrsLung, yrsLymp) %>%
  summarise_all(.funs = ~(sum(. <=10, na.rm = T))) #count cases with dx <=5 yrs ago

#1815 CRC, 4001 Breast, 3086 Prostate, 951 Lung, 699 Lymphoma

#make an appropriate weighting variable:
#per NHIS analytic guidelines, when combining multiple years
#divide individual sampling weights by # waves (here, 15 for 2000-2014)
subData <- subData %>%
  mutate(new_weight = MORTWTSA/15) #mortality weights

#new age as numeric var - it's currently 'haven labelled' which
#works fine to compute ages and such but not for descriptives
subData <- subData %>%
  mutate(age_new = as.numeric(AGE))

#due to concerns over the sample size, collapse across some
#categories of Race and Insurance
subData <- subData %>%
  #military or other together as one 'other' category else leave as is
  mutate(insurance_new = ifelse(InsType %in% c(3,5), 3, InsType),
         #Asian, American Indian/Alaska Native, Other as other
         race_new = ifelse(RaceR %in% c(4,5,6), 4, RaceR))

#make a years since any dx variable
subData <- subData %>%
  rowwise()%>%
  mutate(yrs_any = min(yrsBreast, yrsProst, yrsColorectal, yrsLymp, yrsLung, na.rm =T)) %>%
  ungroup() #undo the rowwise part

#to make data more manageable: pick just variables that we'll
#be using in our analyses
analyticData <- subData %>%
  select(race_new, insurance_new, new_weight,
         PSU, STRATA, cancMort, DEAD, fuTime,
         CRN, lessMed, skipMed, delayMed, BarrierMedR, 
         SEX, BreastCan, LymphomaCan, ProstateCan,
         ColRectCan, LungCan, yrsBreast, yrsLymp, yrsProst,
         yrsLung, yrsColorectal, age_new, SmokeR, EduR, BMI, BMIcat,
         MORTELIG, yrs_any)


#open packages
#install.packages("survey")
#install.packages("tableone")
library(survey)
library(tableone)

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
                             "insurance_new", "BreastCan", "ProstateCan",
                             "LungCan", "LymphomaCan", "ColRectCan", "fuTime", "yrs_any"),
                    strata = "CRN", 
                    data = comp.svy,
                    factorVars = c("race_new", "EduR", "SmokeR", "DEAD",
                                   "cancMort", "skipMed", "delayMed", "lessMed",
                                   "BarrierMedR", "SEX", "insurance_new", 
                                   "BreastCan", "ProstateCan",
                                   "LungCan", "LymphomaCan", "ColRectCan")),
  nonnormal = c("BMI", "age_new", "fuTime", "yrs_any")
)

#open packages needed for survival analysis
#install.packages("survival")
#install.packages("survminer") #for ggcoxdiagnostics
library(survival)
library(survminer)

#create an empty data frame to store results for table output
results <- cbind.data.frame(rep(" ", 6),
                            rep(" ", 6),
                            rep(" ", 6),
                            rep(" ", 6))

#name the columns of the data frame
names(results) <- c("Sample", "Died", "Unadjusted", "Adjusted")
results <- results %>%
  mutate_all(as.character)

#function to add model results into the frame
addResult <- function(mod1, mod2, type, row, data = results){
  data[row,] <- c(type, 
                   #N and % died
                   paste0(mod1$nevent, " (", round(mod1$nevent/mod1$n, 2)*100, "%)"),
                   #HR and 95% CI
                   paste0(round(exp(mod1$coefficients),2), " (", 
                          round(exp(confint(mod1)[1]),2), " - ",
                          round(exp(confint(mod1)[2]),2), ")"),
                   #HR and 95% CI for adjusted model (CRN is 1st coef)
                   paste0(round(exp(mod2$coefficients)[1],2), " (", 
                          round(exp(confint(mod2))[1,1],2), " - ",
                          round(exp(confint(mod2))[1,2],2), ")")
                   )
  return(data)
  
}

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

#add results to the frame
results <- addResult(all.unadj, all.adjusted, "All", 1)

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

#add results to the frame
results <- addResult(br.unadj, br.adjusted, "Breast Cancer", 2)

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


#add results to the frame
results <- addResult(pr.unadj, pr.adjusted, "Prostate Cancer", 3)

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

#add results to the frame
results <- addResult(lu.unadj, lu.adjusted, "Lung Cancer", 4)

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

#add results to the frame
results <- addResult(ly.unadj, ly.adjusted, "Lymphoma", 5)

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

#add results to the frame
results <- addResult(cr.unadj, cr.adjusted, "Colorectal Cancer", 6)

#Assumption 1: Proportional Hazards
#####################################################################
#All Cancers 
(ph.all.unadj <- cox.zph(all.unadj, terms = FALSE)) #meets assumption
(ph.all.adj <- cox.zph(all.adjusted, terms = FALSE)) #meets assumption

#Breast Cancer
(ph.br.unadj <- cox.zph(br.unadj, terms = FALSE)) #meets assumption (p = .087)
(ph.br.adj <- cox.zph(br.adjusted, terms = FALSE)) #meets assumption

#Prostate Cancer
(ph.pr.unadj <- cox.zph(pr.unadj, terms = FALSE)) #meets assumption
(ph.pr.adj <- cox.zph(pr.adjusted, terms = FALSE)) #meets (p = 0.087)

#Lung Cancer
(ph.lu.unadj <- cox.zph(lu.unadj, terms = FALSE)) #meets assumption
(ph.lu.adj <- cox.zph(lu.adjusted, terms = FALSE)) #doesn't meet p = 0.005

#Lymphoma
(ph.ly.unadj <- cox.zph(ly.unadj, terms = FALSE)) #doesn't meet assumption p = 0.016
(ph.ly.adj <- cox.zph(ly.adjusted, terms = FALSE)) #meets assumption

#Colorectal
(ph.cr.unadj <- cox.zph(cr.unadj, terms = FALSE)) #meets assumption
(ph.cr.adj <- cox.zph(cr.adjusted, terms = FALSE)) #meets assumption

#For the models, look visually and see how bad
#the deviations are (or are not)

#all cancers
ggcoxzph(ph.all.unadj)
ggcoxzph(ph.all.adj)

#breast
ggcoxzph(ph.br.unadj)
ggcoxzph(ph.br.adj)

#prostate
ggcoxzph(ph.pr.unadj)
ggcoxzph(ph.pr.adj)

#lung
ggcoxzph(ph.lu.unadj) 
ggcoxzph(ph.lu.adj)
ggcoxzph(ph.lu.adj,var = "factor(CRN)1") #this one violated

#Lymphoma
ggcoxzph(ph.ly.unadj) #this one violated
ggcoxzph(ph.ly.adj) 

#Colorectal
ggcoxzph(ph.cr.unadj)
ggcoxzph(ph.cr.adj)

#none of them are really bad- the violations seem to be largely due to sample size

#In this case, because we're doing complex survey weighting it doesn't really
#make sense to do RMST mainly because I couldn't find any analogous methods
#to appropriately account for the sampling procedure. Instead, estimate by time interval

#for lung model, change time interval to < median / > median
median(lu.svy$variables$fuTime, na.rm = T)

#construct new fu time variable truncated at median weeks
lu.svy <- update(lu.svy, fuTime1 = ifelse(lu.svy$variables$fuTime > 129.9286, 129.9286,
                                          lu.svy$variables$fuTime))

#construct new status variable censoring at median for longtime survivors
lu.svy <- update(lu.svy, cancMort1 = ifelse(lu.svy$variables$fuTime > 129.9286 & 
                                              lu.svy$variables$cancMort == 1, 0,
                                          lu.svy$variables$cancMort))

early.lu <- svycoxph(Surv(fuTime1, cancMort1) ~  factor(CRN) +
                       factor(SEX) + factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrsLung,
                     design = lu.svy)

summary(early.lu)
#see if ph holds
(cox.zph(early.lu, terms = F)) #YES!

#now exclude those followed up for short time
late.lu.svy <- subset(lu.svy, fuTime > 129.9286)

#run model on late survivors
late.lu <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(SEX) + factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrsLung,
                     design = late.lu.svy)

summary(late.lu)
#see if ph holds
(cox.zph(late.lu, terms = F)) #NOPE
#visualize it
ggcoxzph(cox.zph(late.lu, terms = F)) #doesn't look bad
##############################################################################
# Do same for lymphoma: get median follow up
median(ly.svy$variables$fuTime, na.rm = T)

#construct new fu time variable truncated at median weeks
ly.svy <- update(ly.svy, fuTime1 = ifelse(ly.svy$variables$fuTime > 274.0357, 274.0357,
                                          ly.svy$variables$fuTime))

#construct new status variable censoring at median for longtime survivors
ly.svy <- update(ly.svy, cancMort1 = ifelse(ly.svy$variables$fuTime > 274.0357 & 
                                              ly.svy$variables$cancMort == 1, 0,
                                            ly.svy$variables$cancMort))

early.ly <- svycoxph(Surv(fuTime1, cancMort1) ~  factor(CRN) +
                       factor(SEX) + factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrsLymp,
                     design = ly.svy) #seems to have too few cases!

summary(early.ly)
#see if ph holds
(cox.zph(early.ly, terms = F)) #YES!

#now exclyde those followed up for short time
late.ly.svy <- subset(ly.svy, fuTime > 274.0357)

#run model on late survivors
late.ly <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                      factor(SEX) + factor(insurance_new) +
                      age_new + factor(race_new)+
                      yrsLymp,
                    design = late.ly.svy)

summary(late.ly) #again, not enough cases
#see if ph holds
(cox.zph(late.ly, terms = F)) #YES


#########################################################################
#Assumption 2: No influential observations (dfbeta values)
#########################################################################

#make second results frame for those after removing
#influential obs
#create an empty data frame to store results for table output
results2 <- cbind.data.frame(rep(" ", 6),
                            rep(" ", 6),
                            rep(" ", 6),
                            rep(" ", 6))

#name the columns of the data frame
names(results2) <- c("Sample", "Died", "Unadjusted", "Adjusted")
results2 <- results2 %>%
  mutate_all(as.character)

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

results2 <- addResult(all.unadj.1, all.adj.1, row = 1, type = "All", data = results2)

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

results2 <- addResult(br.unadj.1, br.adj.1, row = 2, type = "Breast Cancer", data = results2)

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

results2 <- addResult(pr.unadj.1, pr.adj.1, row = 3, 
                      type = "Prostate Cancer", data = results2)

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

#run coxph and print results : after subsetting there
#weren't enough cases/contrasts to run model
ly.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                     design = comp.svy8)

summary(ly.adj.1) #again, not enough cases.. only 1
table(comp.svy8$variables$CRN, comp.svy8$variables$cancMort)

results2 <- addResult(ly.unadj.1, ly.adj.1, row = 4, type = "Lymphoma",
                      data = results2)

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

#run coxph and print results : after subsetting there
#weren't enough cases/contrasts to run model
lu.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                     design = comp.svy10)
summary(lu.adj.1)

results2 <- addResult(lu.unadj.1, lu.adj.1, row = 5, type = "Lung Cancer",
                      data = results2)

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
summary(cr.unadj.1) 
#converged too early, not enough cases; 0 in fact w/ CRN
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

#run coxph and print results : after subsetting there
#weren't enough cases/contrasts to run model
cr.adj.1 <- svycoxph(Surv(fuTime, cancMort) ~  factor(CRN) +
                       factor(insurance_new) +
                       age_new + factor(race_new)+
                       yrs_any,
                     design = comp.svy12)
summary(cr.adj.1)
table(comp.svy12$variables$CRN, comp.svy12$variables$cancMort)
#convered, but only 11 cases died with CRN

#add what we have to results
results2[6,] <- c("Colorectal Cancer",  paste0(cr.adj.1$nevent, " (", 
                                               round(cr.adj.1$nevent/cr.adj.1$n, 2)*100,
                                               "%)")
                  , NA,  
                  paste0(round(exp(cr.adj.1$coefficients)[1],2), " (", 
                         round(exp(confint(cr.adj.1))[1,1],2), " - ",
                         round(exp(confint(cr.adj.1))[1,2],2), ")"))

#write it out 
write.csv(results2, "data\\results2.csv")
####################################################################
#Assumption 3: Log linearity with continuous predictors
####################################################################
#not looking at unadjusted models bc they have no continuous predictors

#all cancers
mart.all <- residuals(all.adj.1, type="martingale")
plot(comp.svy2$variables$age_new,
     mart.all, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy2$variables$age_new,
                   mart.all), lwd=2, col="blue") #looks linear

plot(comp.svy2$variables$yrs_any,
     mart.all, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy2$variables$yrs_any,
                   mart.all), lwd=2, col="blue") #looks linear

#breast cancer
mart.br <- residuals(br.adj.1, type="martingale")
plot(comp.svy4$variables$age_new,
     mart.br, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy4$variables$age_new,
                   mart.br), lwd=2, col="blue") #looks linear

plot(comp.svy4$variables$yrs_any,
     mart.br, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy4$variables$yrs_any,
                   mart.br), lwd=2, col="blue") #looks linear


#prostate cancer
mart.pr <- residuals(pr.adj.1, type="martingale")
plot(comp.svy6$variables$age_new,
     mart.pr, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy6$variables$age_new,
                   mart.pr), lwd=2, col="blue") #looks pretty good linear

plot(comp.svy6$variables$yrs_any,
     mart.pr, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy6$variables$yrs_any,
                   mart.pr), lwd=2, col="blue") #looks linear


#lymphoma
mart.ly <- residuals(ly.adj.1, type="martingale")
plot(comp.svy8$variables$age_new,
     mart.ly, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy8$variables$age_new,
                   mart.ly), lwd=2, col="blue") #looks pretty good linear

plot(comp.svy8$variables$yrs_any,
     mart.ly, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy8$variables$yrs_any,
                   mart.ly), lwd=2, col="blue") #looks linear


#lung
mart.lu <- residuals(lu.adj.1, type="martingale")
plot(comp.svy10$variables$age_new,
     mart.lu, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy10$variables$age_new,
                   mart.lu), lwd=2, col="blue") #looks pretty good linear

plot(comp.svy10$variables$yrs_any,
     mart.lu, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy10$variables$yrs_any,
                   mart.lu), lwd=2, col="blue") #some deviation but not bad


#colorectal
mart.cr <- residuals(cr.adj.1, type="martingale")
plot(comp.svy12$variables$age_new,
     mart.cr, main="Martingale-residuals for X",
     xlab="age", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy12$variables$age_new,
                   mart.cr), lwd=2, col="blue") #looks pretty good/linear

plot(comp.svy12$variables$yrs_any,
     mart.cr, main="Martingale-residuals for X",
     xlab="yrs since dx", ylab="Residual", pch=20)
lines(loess.smooth(comp.svy12$variables$yrs_any,
                   mart.cr), lwd=2, col="blue") #looks linear


##################################################################################
#Draw the DAGs we were using to pick adjustment vars
library(DiagrammeR)
#make dag
grViz("
	digraph causal {
	
	  # Nodes
	  node [shape = plaintext]

	  U [label = 'Income']
	  D [label = 'CRN']
	  Y [label = 'Mortality']
	  S [label = 'Sex']
	  I [label = 'Insurance']
	  R [label = 'Race']
	  A [label = 'Age']
	  X [label = 'Yrs Since Dx']
	  T [label = 'Cancer Subtype']
	  
	  # Edges
	  edge [color = black,
	        arrowhead = vee]
	  rankdir = LR
	  D -> Y
	  U -> D
	  U -> Y
	  U -> I
	  S -> D
	  S -> Y
	  I -> D
	  I -> Y
	  R -> D
	  R -> Y
	  R -> I
	  A -> I
	  A -> D
	  A -> Y
	  X -> Y
	  T -> D
	  T -> Y
	  R -> T
	  A -> T

	  # Graph
	  graph [overlap = true, fontsize = 10]
	}")

#note that in this context BOTH subtype and income are unmeasured so we can't
#adjust for them (well, technically income is measured but many references commented
#questioning the validity of the imputation procedure, so it's a question of how much
#the model is biased by leaving it out versus biased because of measurement error. Given
#the tradeoff especially given that we had relatively few cases in certain instances,
#we decided to not introduce the biased measure in, hoping that most of income's influence
#in this context is driven through insurance pathways). Why adjust for yrs since diagnosis?
#In this case it's not a confounder but adjusting for it does improve precision.

#write out results so we can put them in presentation easily
write.csv(results, "data\\results.csv")


###########################################################################################
#Response to Reviewers Additions: Test for interaction btwn insurance+CRN

#run model with interaction before adjustment
int.mod.unadj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN)*factor(insurance_new),
                          design = comp.svy)

#print results of model with interaction
summary(int.mod.unadj) 

#none of the levels are sig, so the interaction as a whole
#shouldn't be either... but can confirm this with a quasi-LR test

#according to survey package author (pg 75) here:
# https://faculty.washington.edu/tlumley/old-survey/survey-wss.pdf
#likelihood ratio test aren't available because of the way variance is computed
#instead, use the regTermTest for all levels of a variable
regTermTest(int.mod.unadj, test.terms = ~factor(CRN):factor(insurance_new)) # p= 0.31541

#repeat after adjusting model
#run it after adjustments
int.mod.adj <- svycoxph(Surv(fuTime, cancMort) ~ factor(CRN)*factor(insurance_new) +
                          factor(SEX)+ factor(race_new) + age_new + yrs_any,
                          design = comp.svy)

#print results
summary(int.mod.adj)
#still nothing is significant, again confirm
regTermTest(int.mod.adj, test.terms = ~factor(CRN):factor(insurance_new)) # p= 0.31268 

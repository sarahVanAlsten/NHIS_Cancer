##########################################################################
#Name: Sarah Van Alsten
#Date Created: January 19, 2020
#Dataset used: National Health Interview Survey (years = 2000 - 2014)
#Packages Used: tidyverse, ipumsr
#Purpose: Read in and clean NHIS data
#Last Update: March 21, 2020
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


################################################
#Income categorical
#While categories are not same size, NHIS coding is not consistent in groupings.
#Some potential codings are 10k wide, others are 5k wide, thus the diff gap size btwn categories
subData <- subData %>%
  mutate(IncomeR = ifelse(INCIMP1<5, 0, #<25k
                          ifelse(INCIMP1<22,1, #<45k
                                 ifelse(INCIMP1<52,2, #<75k
                                        ifelse(is.na(INCIMP1),NA, 3)))))#75k +


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
         IncomeR, SEX, BreastCan, LymphomaCan, ProstateCan,
         ColRectCan, LungCan, yrsBreast, yrsLymp, yrsProst,
         yrsLung, yrsColorectal, age_new, SmokeR, EduR, BMI, BMIcat,
         MORTELIG, yrs_any)




#########################################################
#VARIABLES WITH START AND ONGOING DATES- MATCH FIRST OCCURENCE
##########################################################
#THE FOLLOWING VARIABLES ARE CREATED BASED ON LOOKUP TABLES OF CLINICAL CODES USED TO QUERY AN EHR EXTRACT TABLE.
#THREE COLUMNS ARE CREATED FOR EACH METRIC- TIME SINCE EVENT (DaysSince*), EVENT DATE (*Date), AND A BINARY MARKER (Condition_name) SHOWING EVER OCCURENCE

library("zoo")
library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("zoo")
library("data.table")
library("survival")
library("lubridate")
library("tidyverse")
library("tidyselect")

setwd("~/Peek_PERMIT")

calculate_prev <- function(d, column){
  
  column <- quo_name(column)
  
  d %>% 
    select_("PatientID", column) %>%
      distinct() %>%
        rename(diagnosis = !!column) %>%
          summarise(patients_number = length(unique(PatientID)),
                    affected_patients = sum(diagnosis, na.rm = TRUE),
                    prevalence = affected_patients / patients_number)
  
}

load("sir.data2yrsall.rda") #Load the full table
load("SIR_crea.repongoing.rda") 

crea.rep <- crea.rep %>%
  arrange(PatientID, event.date)

sir.data <- sir.data %>% 
  filter(PatientID %in% crea.rep$PatientID) %>%
    filter(!(event.date<as.Date("1900-01-01") | event.date>as.Date("2017-08-01"))) # remove records with impossible 
#######################################################
#Suffix files with a shared symbol to load them simultaneously:
allfiles = list.files(pattern="*1.csv")
for (i in 1:length(allfiles)) assign(allfiles[i], read.csv(allfiles[i]))
#######################################################

#NEPHRECTOMY
sir.data$NephDate<-sir.data$EntryDate 
sir.data$NephDate<-ifelse(!(sir.data$ReadCode  %in%  Nephrectomy1.csv$ReadCode),
                          NA,
                          sir.data$NephDate) 

summary(sir.data$NephDate)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19460000 19930000 20040000 19970000 20100000 20150000 23853452 

smalltab<-sir.data[!is.na(sir.data$NephDate),
                   c("PatientID","NephDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(NephDate) %>%
        slice(which.min(NephDate)) %>%
          as.data.frame
head(first)

# PatientID NephDate
# 1       587 20130811
# 2       605 20150515
# 3      1103 20130312
# 4      2530 20131003
# 5      2693 20050715
# 6      3183 20090428

first$NephDate<-as.Date(as.character(first$NephDate),format="%Y%m%d")
summary(first$NephDate)
# Min.          1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1946-01-01" "1980-01-01" "2001-04-20" "1993-11-23" "2009-08-30" "2015-06-20" 

#crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep <- crea.rep %>%
  left_join(first, by = "PatientID")

crea.rep$DaysSinceNephrectomy<-as.numeric(difftime(strptime(crea.rep$NephDate,format="%Y-%m-%d"),
                                                   strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                                   unit="days"))

summary(crea.rep$DaysSinceNephrectomy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -25820  -10060   -4335   -6108    -811    5627  297935 

crea.rep$DaysSinceNephrectomy<-ifelse(crea.rep$DaysSinceNephrectomy<0,
                                      NA,
                                      crea.rep$DaysSinceNephrectomy)

crea.rep$Nephrectomy<-ifelse(is.na(crea.rep$DaysSinceNephrectomy),
                             0,
                             1)

calculate_prev(d = crea.rep, column = 'Nephrectomy')
#   patients_number affected_patients  prevalence
# 1            6251                26 0.004159335


#RRT
sir.data$RRTDate<-sir.data$EntryDate

sir.data$RRTDate<-ifelse(!sir.data$ReadCode  %in%  Dialysis1.csv$ReadCode,
                         NA,
                         sir.data$RRTDate) 

summary(sir.data$RRTDate)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19900000 20090000 20130000 20110000 20140000 20160000 23853313 

smalltab<-sir.data[!is.na(sir.data$RRTDate),
                   c("PatientID","RRTDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(RRTDate) %>%
        slice(which.min(as.numeric(RRTDate))) %>%
          as.data.frame

head(first)
#   PatientID  RRTDate
# 1      1028 20130925
# 2      1516 20080401
# 3      1667 20060622
# 4      1765 20080929
# 5      1955 20030101
# 6      2104 20121231

first$RRTDate<-as.Date(as.character(first$RRTDate),
                       format="%Y%m%d")

crea.rep <- crea.rep %>%
  left_join(first, by = "PatientID")

crea.rep$DaysSinceRRT<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                strptime(crea.rep$RRTDate,format="%Y-%m-%d"),
                                unit="days") %>%
                          as.numeric()

summary(crea.rep$DaysSinceRRT)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -7492.0  -590.3   148.5   413.0  1390.0  9357.0  290920

crea.rep$DaysSinceRRT<-ifelse(crea.rep$DaysSinceRRT<0,
                              NA,
                              crea.rep$DaysSinceRRT)

crea.rep$RRT<-ifelse(is.na(crea.rep$DaysSinceRRT),0,1)

calculate_prev(d = crea.rep, column = 'RRT')
#   patients_number affected_patients prevalence
# 1            6251                71 0.01135818

#DIABETES
sir.data$DiabDate<-sir.data$EntryDate 

sir.data$DiabDate<-ifelse(!sir.data$ReadCode  %in%  Diabetes1.csv$ReadCode,
                          NA,
                          sir.data$DiabDate) 

smalltab<-sir.data[!is.na(sir.data$DiabDate),
                   c("PatientID","DiabDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(DiabDate) %>%
      slice(which.min(as.numeric(DiabDate))) %>%
        as.data.frame

head(first)
#   PatientID DiabDate
# 1         7 20030501
# 2        18 19821010
# 3        20 20140306
# 4        24 20050316
# 5        56 20050719
# 6        73 20070227

first$DiabDate<-as.Date(as.character(first$DiabDate),format="%Y%m%d")

crea.rep <- crea.rep %>%
  left_join(first, by = "PatientID")

crea.rep$DaysSinceDiabetic<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                     strptime(crea.rep$DiabDate,format="%Y-%m-%d"),
                                     unit="days") %>%
                              as.numeric()

crea.rep$DaysSinceDiabetic<-ifelse(crea.rep$DaysSinceDiabetic<0,
                                   NA,
                                   crea.rep$DaysSinceDiabetic)

crea.rep$Diabetes<-ifelse(is.na(crea.rep$DaysSinceDiabetic),0,1)

calculate_prev(d = crea.rep, column = 'Diabetes')
#   patients_number affected_patients prevalence
# 1            6251              1942  0.3106703


#ATRIAL FIBRILLATION
sir.data$AFDate<-sir.data$EntryDate 

sir.data$AFDate<-ifelse(!(sir.data$ReadCode  %in%  AF1.csv$ReadCode),
                        NA,
                        sir.data$AFDate)

summary(sir.data$AFDate)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19370000 20060000 20100000 20090000 20130000 20160000 23843868

smalltab<-sir.data[!is.na(sir.data$AFDate),
                   c("PatientID","AFDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(AFDate) %>%
        slice(which.min(as.numeric(AFDate))) %>%
          as.data.frame
    
head(first)

#   PatientID   AFDate
# 1         4 20140411
# 2        20 20070214
# 3        25 19931213
# 4        37 20050819
# 5        38 19940521
# 6        41 20120512

first$AFDate<-as.Date(as.character(first$AFDate),format="%Y%m%d")

crea.rep <- crea.rep %>%
  left_join(first, by = "PatientID")

crea.rep$DaysSinceAF<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                               strptime(crea.rep$AFDate,format="%Y-%m-%d"),
                               unit="days") %>%
                         as.numeric()

summary(crea.rep$DaysSinceAF)

crea.rep$DaysSinceAF<-ifelse(crea.rep$DaysSinceAF<0,
                             NA,
                             crea.rep$DaysSinceAF)

crea.rep$AF<-ifelse(is.na(crea.rep$DaysSinceAF),0,1)

calculate_prev(d = crea.rep, column = 'AF')

#   patients_number affected_patients prevalence
# 1            6251              2503  0.4004159


#IHD - Ischemic Heart Disease
sir.data$IHDDate<-sir.data$EntryDate

sir.data$IHDDate<-ifelse(!(sir.data$ReadCode  %in%  IHD1.csv$ReadCode)&
                           !(sir.data$ReadCode  %in%  IHD1.csv$MedCode),
                         NA,
                         sir.data$IHDDate)

summary(sir.data$IHDDate)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19570000 20030000 20070000 20070000 20110000 20160000 23819919 

smalltab<-sir.data[!is.na(sir.data$IHDDate),
                   c("PatientID","IHDDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(IHDDate) %>%
        slice(which.min(as.numeric(IHDDate))) %>%
          as.data.frame

head(first)
# PatientID  IHDDate
# 1         1 20091223
# 2         7 20070619
# 3         9 20060124
# 4        24 20140420
# 5        25 19850101
# 6        27 20070806

crea.rep <- crea.rep %>%
  left_join(first, by = "PatientID")

crea.rep$IHDDate<-as.Date(as.character(crea.rep$IHDDate),format="%Y%m%d")

crea.rep$DaysSinceIHD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                strptime(crea.rep$IHDDate,format="%Y-%m-%d"),
                                unit="days") %>%
                        as.numeric()

crea.rep$DaysSinceIHD<-ifelse(crea.rep$DaysSinceIHD<0,
                              NA,
                              crea.rep$DaysSinceIHD)

crea.rep$IHD<-ifelse(is.na(crea.rep$DaysSinceIHD),
                     0,
                     1)

calculate_prev(d = crea.rep, column = 'IHD')
#   patients_number affected_patients prevalence
# 1            6251              3957  0.6330187

#PERIPHERAL VASCULAR DISEASE
sir.data$PVDDate<-sir.data$EntryDate

sir.data$PVDDate<-ifelse(!(sir.data$ReadCode  %in%  PVD1.csv$ReadCode),
                         NA,
                         sir.data$PVDDate)

summary(sir.data$PVDDate)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19490000 20010000 20070000 20050000 20110000 20160000 23846064 

smalltab<-sir.data[!is.na(sir.data$PVDDate),c("PatientID","PVDDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(PVDDate) %>%
        slice(which.min(as.numeric(PVDDate))) %>%
          as.data.frame

head(first)

#   PatientID  PVDDate
# 1         7 20020814
# 2         9 20100216
# 3        20 19950524
# 4        28 19900306
# 5        38 20140819
# 6        52 20120619

crea.rep <- crea.rep %>%
              left_join(first, by = "PatientID")

crea.rep$PVDDate<-as.Date(as.character(crea.rep$PVDDate),format="%Y%m%d")

crea.rep$DaysSincePVD <- difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                  strptime(crea.rep$PVDDate,format="%Y-%m-%d"),
                                  unit="days") %>%
                            as.numeric()

summary(crea.rep$DaysSincePVD)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   -8219      47    1974    2832    4642   23580  162130

crea.rep$DaysSincePVD<-ifelse(crea.rep$DaysSincePVD<0,
                              NA,
                              crea.rep$DaysSincePVD)

crea.rep$PVD<-ifelse(is.na(crea.rep$DaysSincePVD),
                     0,
                     1)

calculate_prev(d = crea.rep, column = 'PVD')
#   patients_number affected_patients prevalence
# 1            6251              2444  0.3909774

#RENAL MALIGNANCY
sir.data$RMALDate<-sir.data$EntryDate

sir.data$RMALDate<-ifelse(!sir.data$ReadCode  %in%  RM1.csv$ReadCode,
                          NA,
                          sir.data$RMALDate)

summary(sir.data$RMALDate)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19830000 20070000 20100000 20090000 20130000 20160000 23853478 

smalltab<-sir.data[!is.na(sir.data$RMALDate),
                   c("PatientID","RMALDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(RMALDate) %>%
        slice(which.min(as.numeric(RMALDate))) %>%
          as.data.frame

head(first)
  
#   PatientID RMALDate
# 1       587 20130328
# 2       605 20150611
# 3      1797 20150327
# 4      2347 20070730
# 5      2393 19940929
# 6      2530 20130930

crea.rep <- crea.rep %>%
              left_join(first, by = "PatientID")

crea.rep$RMALDate<-as.Date(as.character(crea.rep$RMALDate),format="%Y%m%d")

crea.rep$DaysSinceRenMal<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                   strptime(crea.rep$RMALDate,format="%Y-%m-%d"),
                                   unit="days") %>%
                            as.numeric()

crea.rep$DaysSinceRenMal<-ifelse(crea.rep$DaysSinceRenMal<0,
                                 NA,
                                 crea.rep$DaysSinceRenMal)

crea.rep$RenalMalignancy<-ifelse(is.na(crea.rep$DaysSinceRenMal),
                                 0,
                                 1)

calculate_prev(d = crea.rep, column = 'RenalMalignancy')

#   patients_number affected_patients  prevalence
# 1            6251                40 0.006398976


#RENAL TRANSPLANT
sir.data$RTDate<-sir.data$EntryDate

sir.data$RTDate<-ifelse(!sir.data$ReadCode  %in%  transplant1.csv$ReadCode,
                        NA,
                        sir.data$RTDate)

summary(sir.data$RTDate)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19890000 20090000 20130000 20100000 20150000 20160000 23853532

smalltab<-sir.data[!is.na(sir.data$RTDate),
                   c("PatientID","RTDate")]

first<-smalltab %>%
        group_by(PatientID) %>%  
          arrange(RTDate) %>% 
            slice(which.min(as.numeric(RTDate))) %>%
              as.data.frame    

head(first)

#   PatientID   RTDate
# 1      1667 20081114
# 2      3056 20150108
# 3      3887 20100610
# 4      5585 20120323
# 5     12468 20141229
# 6     15158 20080606

crea.rep<- crea.rep %>% 
            left_join(first, by = "PatientID")

crea.rep$RTDate<-as.Date(as.character(crea.rep$RTDate),format="%Y%m%d")

crea.rep$DaysSinceRT<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                               strptime(crea.rep$RTDate,format="%Y-%m-%d"),
                               unit="days") %>%
                        as.numeric()

summary(crea.rep$DaysSinceRT)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -5763.0 -1345.0  -150.0   736.5  1578.0 10120.0  299720

crea.rep$DaysSinceRT<-ifelse(crea.rep$DaysSinceRT<0,
                             NA,
                             crea.rep$DaysSinceRT)

crea.rep$RenalTransplant<-ifelse(is.na(crea.rep$DaysSinceRT),
                                 0,
                                 1)

calculate_prev(d = crea.rep, column = 'RenalTransplant')
#   patients_number affected_patients  prevalence
# 1            6251                13 0.002079667

#CLD
sir.data$CLDDate<-sir.data$EntryDate 

sir.data$CLDDate<-ifelse(!sir.data$ReadCode  %in%  Liver1.csv$ReadCode,
                         NA,
                         sir.data$CLDDate) 

summary(sir.data$CLDDate)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19680000 20060000 20100000 20090000 20140000 20160000 23853388 

smalltab<-sir.data[!is.na(sir.data$CLDDate) & 
                     sir.data$PatientID %in% crea.rep$PatientID,
                   c("PatientID","CLDDate")]

first<-smalltab %>%
    group_by(PatientID) %>%
      arrange(CLDDate) %>%
        slice(1L) %>%
          as.data.frame

head(first)

#   PatientID  CLDDate
# 1         9 20001201
# 2        74 20130517
# 3       253 20111126
# 4       317 20030901
# 5       336 20130327
# 6       417 20140127

crea.rep <- crea.rep %>% 
              left_join(first, by ="PatientID")

crea.rep$CLDDate<-as.Date(as.character(crea.rep$CLDDate),format="%Y%m%d")

crea.rep$DaysSinceCLD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),
                                strptime(crea.rep$CLDDate,format="%Y-%m-%d"),
                                unit="days") %>%
                        as.numeric()

summary(crea.rep$DaysSinceCLD)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -7602.0 -1245.0   114.0   462.4  1581.0 17720.0  294975

crea.rep$DaysSinceCLD<-ifelse(crea.rep$DaysSinceCLD<0,
                              NA,
                              crea.rep$DaysSinceCLD)

crea.rep$CLD<-ifelse(crea.rep$DaysSinceCLD>=0,
                     1,
                     0)

calculate_prev(d = crea.rep, column = 'CLD')

#   patients_number affected_patients prevalence
# 1            6251               110 0.01759718

#MATERNITY 

sir.data$Maternity<-ifelse(sir.data$ReadCode  %in%  Maternity1.csv$ReadCode,
                           1,
                           0) 

smalltab<-sir.data[sir.data$Maternity==1 & sir.data$PatientID %in% crea.rep$PatientID,
                   c("PatientID","Maternity","EntryDate")]

names(smalltab)[3]<-"MatDate1"

smalltab<-smalltab[smalltab$MatDate1>="2007-01-01",]

first<-smalltab %>%
   group_by(PatientID) %>%  
      slice(which.min(as.numeric(MatDate1))) %>%
        as.data.frame

head(first)
#   PatientID Maternity MatDate1
# 1       161         1 20140501
# 2       388         1 20141124
# 3       426         1 20111104
# 4       901         1 20090911
# 5      1070         1 20070423
# 6      1323         1 20150918

#MARKS  FIRST RECORDED PREGANANCY DATE FOR THE PATIENT'S FIRST POST-2007 BABY- NOT NECESSARILY THE FIRST CHILD.

names(smalltab)[3]<-"MatDate2"

smalltab<-merge(smalltab[,c(1,3)],
                first[,c(1,3)],
                all.x=TRUE)

smalltab$MatDate2<-as.Date(as.character(smalltab$MatDate2),"%Y%m%d")

smalltab$MatDate1<-as.Date(as.character(smalltab$MatDate1),"%Y%m%d")

smalltab$d<-as.numeric(smalltab$MatDate2-smalltab$MatDate1)

second<-smalltab[smalltab$d>=243,]

s<-second %>%
   group_by(PatientID) %>%  
      slice(which.min(as.numeric(MatDate2))) %>%
      as.data.frame
        head(s)
        
smalltab2<-sir.data[sir.data$Maternity==1 &
                      sir.data$PatientID %in% crea.rep$PatientID,
                    c("PatientID","Maternity","EntryDate")]

names(smalltab2)[3]<-"MatDate3"

smalltab2$MatDate3<-as.Date(as.character(smalltab2$MatDate3),"%Y%m%d")

smalltab2<-smalltab2[smalltab2$MatDate3>="2007-01-01",]

smalltab2<-merge(smalltab2[,c(1,3)],s,all.x=TRUE)

smalltab2<-unique(smalltab2)

smalltab2$d2<-as.numeric(smalltab2$MatDate3-smalltab2$MatDate2)

third<-smalltab2[smalltab2$d2>=243,]

third<-unique(third)
   
crea.rep<-merge(crea.rep,unique(smalltab2[,c(1,3,4)]),all.x=TRUE)

crea.rep$EstPregnant<-ifelse(as.numeric(crea.rep$event.date-crea.rep$MatDate1)<243,
                             1,
                             NA)

crea.rep$EstPregnant<-ifelse(as.numeric(crea.rep$event.date-crea.rep$MatDate2)<243,
                             1,
                             crea.rep$EstPregnant)

summary(crea.rep$EstPregnant)

save(crea.rep, file = "SIR_crea.repongoing_afterconditions.rda")
#IF APPLYING A DATE RANGE EXCLUDION, DROP OUT OF RANGE ENTRIES:

crea.rep$BNP_DF<-ifelse(crea.rep$BNPDateFlag==1,
                        NA,
                        crea.rep$BNP)

crea.rep$NTPROBNP_DF<-ifelse(crea.rep$NTPROBNPDateFlag==1,
                             NA,
                             crea.rep$NTPROBNP)

crea.rep$SBP_DF<-ifelse(crea.rep$SBPDateFlag==1,
                        NA,
                        crea.rep$SBP)

crea.rep$DBP_DF<-ifelse(crea.rep$DBPDateFlag==1,
                        NA,
                        crea.rep$DBP)

crea.rep$HeartRate_DF<-ifelse(crea.rep$HeartRateDateFlag==1,
                              NA,
                              crea.rep$HeartRate)

crea.rep$BMI_DF<-ifelse(crea.rep$BMIDateFlag==1,
                        NA,
                        crea.rep$BMI)

crea.rep$SerumAlbumin_DF<-ifelse(crea.rep$SerumAlbuminDateFlag==1,
                                 NA,
                                 crea.rep$SerumAlbumin)

crea.rep$UrineAlbumin_DF<-ifelse(crea.rep$UrineAlbuminDateFlag==1,
                                 NA,
                                 crea.rep$UrineAlbumin)

crea.rep$UACratio_DF<-ifelse(crea.rep$UACDateFlag==1,
                             NA,
                             crea.rep$UACratio)

crea.rep$Haemoglobin_DF<-ifelse(crea.rep$HaemDateFlag==1,
                                NA,
                                crea.rep$Haemoglobin)

crea.rep$MCV_DF<-ifelse(crea.rep$MCVDateFlag==1,
                        NA,
                        crea.rep$MCV)

crea.rep$SerPotassium_DF<-ifelse(crea.rep$SerPotDateFlag==1,
                                 NA,
                                 crea.rep$SerPotassium)

crea.rep$SerumSodium_DF<-ifelse(crea.rep$SerSodDateFlag==1,
                                NA,
                                crea.rep$SerumSodium)

crea.rep$BUN_DF<-ifelse(crea.rep$BUNDateFlag==1,
                        NA,
                        crea.rep$BUN)

crea.rep$UricAcid_DF<-ifelse(crea.rep$UricAcidDateFlag==1,
                             NA,
                             crea.rep$UricAcid)

save(crea.rep, file = "SIR_crea.repongoing_afterconditions_lab_flags_applied.rda")

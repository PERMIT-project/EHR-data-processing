#memory.size(70000) #Assign sufficient memory to R to accomplish tasks required

library("zoo")
#library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("tidyverse")
library("zoo")
library("data.table")
library("survival")
library("lubridate")

#######################################################

load("SIR_crea.rep2yrsall.rda") #primary table
load("sir.data2yrsall.rda") #full table containing all patient data

sir.data<-sir.data[sir.data$PatientID %in% crea.rep$PatientID,] # keep only patients that respect incl criteria
sir.data$CodeValue<-as.numeric(as.character(sir.data$CodeValue)) # convert CodeValue to numeric
sir.data<-sir.data[!is.na(sir.data$CodeValue),] # remove NAs
sir.data <- sir.data %>% 
  filter(!(event.date<as.Date("1900-01-01") | event.date>as.Date("2017-08-01"))) # remove records with impossible dates

summary(sir.data$event.date)

#######################################################
#Suffix files with a shared symbol to load them simultaneously:
allfiles = list.files(pattern="*1.csv")
for (i in 1:length(allfiles)) assign(allfiles[i], read.csv(allfiles[i]))
#######################################################

#CODE CONDITIONS REQUIRING NUMERIC VALUES TO BE ADDED, NEAREST IF WITHIN X TIME PERIOD
#NOTE THAT COERCION OF TIBBLES TO DATAFRAME FORMAT MAY NOT BE MAINTAINED IF YOU SAVE R OBJECTS
#TIBBLE FORMATTING CAUSING ISSUES CAN ALSO BE CLEARED BY SAVING MIDWAY POINTS AS CSV FILES

#NEAREST PRIOR MEAN DAILY SERUM SODIUM #DATE FLAG 30 DAYS
unique(sir.data$CodeUnits[sir.data$ReadCode %in% SerumSodium1.csv$ReadCode])
#[1] mmol/L mmol/l        None   132    %      127    134    138    mmol   1      136
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumSodium1.csv$ReadCode &
                        !sir.data$CodeUnits=="%" & sir.data$CodeValue>=20 & 
                          sir.data$CodeValue<=3000, 
                      paste(sir.data$CodeValue), #why using paste?
                      NA)

smalltab<-sir.data[!is.na(sir.data$temp),c("PatientID","CodeValue","event.date")]
columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumSodium=mean(CodeValue)) %>% 
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$SerumSodium<-first[indx1, "SerumSodium"]
crea.rep$SerSodDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
crea.rep$SerumSodium<-unlist(crea.rep$SerumSodium)

summary(crea.rep)

#CLOSEST DAILY BMI MEAN #1YR DATE FLAG
sir.data$temp<-as.numeric(ifelse(sir.data$ReadCode %in% BMI1.csv$ReadCode & 
                                   sir.data$CodeValue>=10 & 
                                    sir.data$CodeValue<=70,
                                 as.numeric(paste(sir.data$CodeValue)),
                                 NA))

sir.data$height<-as.numeric(ifelse(sir.data$ReadCode %in% Height1.csv$ReadCode,
                                   as.numeric(paste(sir.data$CodeValue)),
                                   NA))

unique(sir.data$CodeUnits[sir.data$ReadCode %in% Height1.csv$ReadCode])
#[1] cm    m 

sir.data$height<-ifelse(!is.na(sir.data$height)&sir.data$CodeUnits=="cm",
                        sir.data$height/100,
                        sir.data$height)

unique(sir.data$CodeUnits[sir.data$ReadCode %in% Weight1.csv$ReadCode])
# [1] Kg          kg    kg/m2 None 

sir.data$temp<-ifelse(is.na(sir.data$temp)&sir.data$ReadCode %in% Weight1.csv$ReadCode & sir.data$CodeUnits=="kg/m2",
                      sir.data$CodeValue,
                      sir.data$temp)
#Apply max height across dataset

htab<-sir.data[!is.na(sir.data$height),c("PatientID","CodeValue","event.date")]

columns1=names(htab[c(1,3)])

dots<-lapply(columns1, as.symbol)
firstH <-htab %>% 
  group_by_(.dots=dots) %>%
  summarize(Height=max(CodeValue)) %>%
  as.data.frame
sir.data<-merge(sir.data,firstH,all.x=TRUE)

sir.data$weight<-as.numeric(ifelse(sir.data$ReadCode %in% Weight1.csv$ReadCode & !sir.data$CodeUnits=="kg/m2",
                                   as.numeric(paste(sir.data$CodeValue)),
                                   NA))


sir.data$temp<-ifelse(is.na(sir.data$temp),(as.numeric(sir.data$weight)/(as.numeric(sir.data$height))^2),sir.data$temp)

BMItab<-sir.data[sir.data$temp>=10 & sir.data$temp<=70,c("PatientID","temp","event.date")]
columns1=names(BMItab[c(1,3)])
dots<-lapply(columns1, as.symbol)
firstA <-BMItab %>% 
  group_by_(.dots=dots) %>%
  summarize(BMI=mean(temp)) %>%
  as.data.frame

indx1 <- neardate(crea.rep$PatientID, firstA$PatientID, crea.rep$event.date, firstA$event.date, 
                  best="prior")
crea.rep$BMI<-firstA[indx1, "BMI"]
crea.rep$BMIDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - firstA$event.date[indx1])) >365, 1, 0)
crea.rep$BMI<-unlist(crea.rep$BMI)

summary(crea.rep)

#MEAN DAILY SERUM URIC ACID #1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UricAcid1.csv$ReadCode, 
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

UA<-sir.data[!is.na(sir.data$temp) & !is.na(sir.data$PatientID),]
unique(UA$CodeUnits)
sir.data$temp<-ifelse((sir.data$CodeUnits=="umol/L"|sir.data$CodeUnits=="umol/l") & sir.data$ReadCode %in% UricAcid1.csv$ReadCode,sir.data$temp/1000,sir.data$temp)
sir.data$temp<-ifelse((sir.data$CodeUnits=="iu/L"|sir.data$CodeUnits=="IU/L"|sir.data$CodeUnits=="None") & sir.data$ReadCode %in% UricAcid1.csv$ReadCode,NA,sir.data$temp)
sir.data$temp<-ifelse(sir.data$CodeValue>=0.001 & sir.data$CodeValue<=1, sir.data$temp,NA)

smalltab<-sir.data[!is.na(sir.data$temp),c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
firstU <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UricAcid=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, firstU$PatientID, crea.rep$event.date, firstU$event.date, 
                 best="prior")
crea.rep$UricAcid<-firstU[indx1, "UricAcid"]
crea.rep$UricAcidDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - firstU$event.date[indx1])) >365, 1, 0)
crea.rep$UricAcid<-unlist(crea.rep$UricAcid)

summary(crea.rep)

#MEAN DAILY BLOOD UREA NITROGEN #30 DAY DATE FLAG
table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% BUN1.csv$ReadCode]))
sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)
sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode & as.numeric(sir.data$CodeUnits)>=1,
                      as.numeric(sir.data$CodeUnits),
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$CodeValue>=1&as.numeric(sir.data$CodeValue)<=50,c("PatientID","CodeValue","event.date")]
summary(smalltab$CodeValue)

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(BUN=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$BUN<-first[indx1, "BUN"]
crea.rep$BUNDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)

crea.rep$BUN<-unlist(crea.rep$BUN)
crea.rep$BUNDateFlag<-ifelse(is.na(crea.rep$BUNDateFlag),0,crea.rep$BUNDateFlag)
crea.rep$BUN_DF<-ifelse(crea.rep$BUNDateFlag==0,crea.rep$BUN,NA)

summary(crea.rep)

#MEAN DAILY SERUM POTASSIUM #1 MONTH DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode])
# sir.data$temp<-ifelse((is.na(sir.data$temp)|sir.data$temp == 0)&sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode & as.numeric(as.character(sir.data$CodeUnits))>0,
#                       as.numeric(sir.data$CodeUnits),
#                       sir.data$temp)
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode & (sir.data$CodeUnits=="mL/min"),
                      NA,
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=2&sir.data$temp<=10,c("PatientID","CodeValue","event.date")]

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumPotassium=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$SerPotassium<-first[indx1, "SerumPotassium"]
crea.rep$SerPotDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
crea.rep$SerPotassium<-unlist(crea.rep$SerPotassium)

summary(crea.rep)

#MEAN DAILY HEART RATE #30 DAYS
sir.data$temp<-ifelse(sir.data$ReadCode %in% HeartRate1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

unique(sir.data$CodeUnits[sir.data$ReadCode %in% HeartRate1.csv$ReadCode])

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=20&sir.data$temp<=200,c("PatientID","CodeValue","event.date")]

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(HeartRate=mean(CodeValue)) %>%
  as.data.frame 
indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$HeartRate<-first[indx1, "HeartRate"]
crea.rep$HeartRateDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
crea.rep$HeartRate<-unlist(crea.rep$HeartRate)
crea.rep$HeartRateDateFlag<-ifelse(is.na(crea.rep$HeartRateDateFlag),0,crea.rep$HeartRateDateFlag)
crea.rep$HeartRate_DF<-ifelse(crea.rep$HeartRateDateFlag==0,crea.rep$HeartRate,NA)

summary(crea.rep)

#MEAN BNP #1 YEAR
sir.data$temp<-ifelse(sir.data$ReadCode %in% BNP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
#1ng/L=1pg/ml
unique(sir.data$CodeUnits[sir.data$ReadCode %in% BNP1.csv$ReadCode])
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=1&sir.data$temp<=1000,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(BNP=mean(as.numeric(CodeValue))) %>%
  as.data.frame 
indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$BNP<-first[indx1, "BNP"]
crea.rep$BNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$BNP<-unlist(crea.rep$BNP)
crea.rep$BNPDateFlag<-ifelse(is.na(crea.rep$BNPDateFlag),0,crea.rep$BNPDateFlag)
crea.rep$BNP_DF<-ifelse(crea.rep$BNPDateFlag==0,crea.rep$BNP,NA)

summary(crea.rep)

#MEAN NT-PRO BNP #1 YEAR
sir.data$temp<-ifelse(sir.data$ReadCode %in% NTPROBNP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% NTPROBNP1.csv$ReadCode])
#1ng/L=1pg/ml
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=1&sir.data$temp<=6000,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(NTPROBNP=mean(as.numeric(CodeValue))) %>%
  as.data.frame 
indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$NTPROBNP<-first[indx1, "NTPROBNP"]
crea.rep$NTPROBNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$NTPROBNPDateFlag<-ifelse(is.na(crea.rep$NTPROBNPDateFlag),0,crea.rep$NTPROBNPDateFlag)
crea.rep$NTPROBNP<-unlist(crea.rep$NTPROBNP)
crea.rep$NTPROBNP_DF<-ifelse(crea.rep$NTPROBNPDateFlag==0,crea.rep$NTPROBNP,NA)

summary(crea.rep)

#MIN DAILY SYSTOLIC BP #30 DAYS
sir.data$SBP<-ifelse(sir.data$ReadCode %in% SBP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
sir.data$temp<-sir.data$SBP
unique(sir.data$CodeUnits[sir.data$ReadCode %in% SBP1.csv$ReadCode])
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=30&sir.data$temp<=300,c("PatientID","SBP","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  slice(which.min(as.numeric(SBP))) %>%
  ungroup()%>%
  as.data.frame

indx2 <- neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                  best="prior")
crea.rep$SBP<-first[indx2, "SBP"]
crea.rep$SBPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx2])) >30, 1, 0)
crea.rep$SBP<-unlist(crea.rep$SBP)

summary(crea.rep)

#MIN DAILY DIASTOLIC BP #30 DAYS
sir.data$DBP<-ifelse(sir.data$ReadCode %in% DBP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
sir.data$temp<-sir.data$DBP
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=20&sir.data$temp<=200,c("PatientID","DBP","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  slice(which.min(as.numeric(DBP))) %>%
  ungroup()%>%
  as.data.frame

indx2 <- neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                  best="prior")

crea.rep$DBP<-first[indx2, "DBP"]
crea.rep$DBPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx2])) >30, 1, 0)
crea.rep$DBP<-unlist(crea.rep$DBP)

summary(crea.rep)

#MEAN DAILY SERUM ALBUMIN #`1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode])
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=10&sir.data$temp<=60,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumAlbumin=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$SerumAlbumin<-first[indx1, "SerumAlbumin"]
crea.rep$SerumAlbuminDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - firstU$event.date[indx1])) >365, 1, 0)
crea.rep$SerumAlbumin<-unlist(crea.rep$SerumAlbumin)

summary(crea.rep)

#MAX DAILY URINE ALBUMIN 1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% UAlbumin1.csv$ReadCode])
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode&sir.data$CodeUnits=="g/L",sir.data$temp*1000,sir.data$temp)
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=0&sir.data$temp<=1000,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UrineAlbumin=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$UrineAlbumin<-first[indx1, "UrineAlbumin"]
crea.rep$UrineAlbuminDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$UrineAlbumin<-unlist(crea.rep$UrineAlbumin)

summary(crea.rep)

#MEAN DAILY URINE ALBUMIN CREATININE RATIO #1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UACR1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% UACR1.csv$ReadCode])
sir.data$temp<-ifelse(sir.data$ReadCode %in% UACR1.csv$ReadCode&sir.data$CodeUnits=="ratio"|sir.data$CodeUnits=="None"|sir.data$CodeUnits=="U/ml"|sir.data$CodeUnits=="GPL",NA,sir.data$temp)
sir.data$temp<-ifelse(is.na(sir.data$temp)&sir.data$ReadCode %in% UACR1.csv$ReadCode & as.numeric(sir.data$CodeUnits)>0,as.numeric(sir.data$CodeUnits),sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=0&sir.data$temp<=3000,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UACratio=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$UACratio<-first[indx1, "UACratio"]
crea.rep$UACDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$UACratio<-unlist(crea.rep$UACratio)

summary(crea.rep)

#MAX DAILY MEAN CORPUSCULAR VOLUME #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% MCV1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
unique(sir.data$CodeUnits[sir.data$ReadCode %in% MCV1.csv$ReadCode])
sir.data$temp<-ifelse(is.na(sir.data$temp)&sir.data$ReadCode %in% MCV1.csv$ReadCode & as.numeric(sir.data$CodeUnits)>0,as.numeric(sir.data$CodeUnits),sir.data$temp)

#1femtoliter=1 cubic micron
smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=50&sir.data$temp<=150,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(MCV=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$MCV<-first[indx1, "MCV"]
crea.rep$MCVDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >120, 1, 0)
crea.rep$MCV<-unlist(crea.rep$MCV)

summary(crea.rep)

#MAX DAILY HAEMOGLOBIN #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

unique(sir.data$CodeUnits[sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode])

sir.data$temp<-ifelse(is.na(sir.data$temp)&sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode & as.numeric(sir.data$CodeUnits)>0,
                      as.numeric(sir.data$CodeUnits),
                      sir.data$temp)

sir.data$temp<-ifelse(!is.na(sir.data$temp)&sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode &(sir.data$CodeUnits=="g/dl"|sir.data$CodeUnits=="g/dL")& sir.data$temp < 25,
                      sir.data$temp*10,
                      sir.data$temp)


sir.data$temp<-ifelse(is.na(sir.data$temp)&sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode&(sir.data$CodeUnits=="mmol/mol"|sir.data$CodeUnits=="None"),
                      NA,
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=30&sir.data$temp<=260,c("PatientID","CodeValue","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(Haemoglobin=mean(CodeValue)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$Haemoglobin<-first[indx1, "Haemoglobin"]
crea.rep$HaemDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >120, 1, 0)
crea.rep$Haemoglobin<-unlist(crea.rep$Haemoglobin)
#save(crea.rep, file = "crea.repongoing.rda")
summary(crea.rep)

save(crea.rep, file = "SIR_crea.repongoing.rda")

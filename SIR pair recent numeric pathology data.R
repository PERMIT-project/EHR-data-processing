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

sir.data <- sir.data[sir.data$PatientID %in% crea.rep$PatientID,] # keep only patients that respect incl criteria
sir.data$CodeValue <- as.numeric(as.character(sir.data$CodeValue)) # convert CodeValue to numeric
sir.data <- sir.data[!is.na(sir.data$CodeValue),] # remove NAs
sir.data <- sir.data %>% 
  filter(!(event.date<as.Date("1900-01-01") | event.date>as.Date("2017-08-01"))) # remove records with impossible dates / does it make sense to keep records until 1900?

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

sir.data$height<-as.numeric(ifelse((sir.data$ReadCode %in% Height1.csv$ReadCode),
                                   as.numeric(paste(sir.data$CodeValue)),
                                   NA))

sir.data$height <- ifelse(!is.na(sir.data$height) & sir.data$height == 0, # remove zeros
                          NA,
                          sir.data$height)

unique(sir.data$CodeUnits[sir.data$ReadCode %in% Height1.csv$ReadCode])
#[1] cm    m 

while(sum(sir.data$height >= 3 & !is.na(sir.data$height)) > 0){ # loop until there is data to convert from cm to m
  
  sir.data$height<-ifelse(!is.na(sir.data$height) &
                            (sir.data$height >= 3),
                          sir.data$height/100,
                          sir.data$height)
}

summary(sir.data$height)

unique(sir.data$CodeUnits[sir.data$ReadCode %in% Weight1.csv$ReadCode])
# [1] Kg          kg    kg/m2 None 

sir.data$temp<-ifelse(is.na(sir.data$temp)&
                        sir.data$ReadCode %in% Weight1.csv$ReadCode &
                          sir.data$CodeUnits=="kg/m2",
                      sir.data$CodeValue,
                      sir.data$temp)
#Apply max height across dataset

htab<-sir.data[!is.na(sir.data$height),c("PatientID","height","event.date")]

columns1=names(htab[c(1,3)])

dots<-lapply(columns1, as.symbol)
firstH <-htab %>% 
  group_by_(.dots=dots) %>%
  summarize(Height=max(height)) %>%
  as.data.frame

#sir.data<-merge(sir.data,firstH,all.x=TRUE)
sir.data <- sir.data %>%
  left_join(firstH, by = c("PatientID", "event.date"))

sir.data$weight<-as.numeric(ifelse(sir.data$ReadCode %in% Weight1.csv$ReadCode & 
                                     !sir.data$CodeUnits=="kg/m2",
                                   as.numeric(paste(sir.data$CodeValue)),
                                   NA))


sir.data$temp<-ifelse(is.na(sir.data$temp) & !is.na(sir.data$weight) & !is.na(sir.data$Height), # if bmi not found as a record
                      (as.numeric(sir.data$weight)/(as.numeric(sir.data$Height))^2), # calculate bmi with height and weight
                      sir.data$temp)

BMItab<-sir.data[sir.data$temp>=10 & sir.data$temp<=70 & !is.na(sir.data$temp),c("PatientID","temp","event.date")] # remove NAs and implausible values

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

UA<-sir.data[!is.na(sir.data$temp) & !is.na(sir.data$PatientID),] # select only UA data
summary(UA)
unique(UA$CodeUnits)
#[1] mmol/L umol/L IU/L   None   iu/L          umol/l mmol  
sir.data$temp<-ifelse((sir.data$CodeUnits=="umol/L"|sir.data$CodeUnits=="umol/l") & 
                        sir.data$ReadCode %in% UricAcid1.csv$ReadCode,
                      sir.data$temp/1000,
                      sir.data$temp)

sir.data$temp<-ifelse((sir.data$CodeUnits=="iu/L"|sir.data$CodeUnits=="IU/L"|sir.data$CodeUnits=="None") &
                        sir.data$ReadCode %in% UricAcid1.csv$ReadCode,
                      NA,
                      sir.data$temp)

sir.data$temp<-ifelse(sir.data$temp>=0.001 & sir.data$temp<=1, sir.data$temp,NA)

smalltab<-sir.data[!is.na(sir.data$temp),c("PatientID","temp","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
firstU <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UricAcid=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, firstU$PatientID, crea.rep$event.date, firstU$event.date, 
                 best="prior")

crea.rep$UricAcid<-firstU[indx1, "UricAcid"]
crea.rep$UricAcidDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - firstU$event.date[indx1])) >365, # control if na?
                                   1, 
                                   0)
crea.rep$UricAcid<-unlist(crea.rep$UricAcid)

summary(crea.rep)

#MEAN DAILY BLOOD UREA NITROGEN #30 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% BUN1.csv$ReadCode]), useNA = "ifany") 

# there are values in the units column let's put them in temp if temp is na (e.g. there is no result)



sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode & 
                        as.numeric(as.character(sir.data$CodeUnits))>=1 & 
                        (!is.na(sir.data$temp) | sir.data$temp == 0),
                      as.numeric(as.character(sir.data$CodeUnits)),
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp) & 
                     sir.data$temp>=1 & 
                      as.numeric(sir.data$temp)<=50,
                   c("PatientID","temp","event.date")]

summary(smalltab$temp)

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(BUN=mean(temp)) %>%
  as.data.frame

summary(first)

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
table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode]))

#              %     5.6  mL/min  mmol/l  mmol/L    None     3.6     4.5  mmoL/L mmol/L% 
#   1953       1       1       1    1982  420996     182       2       1       7       2 

sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode & (!(sir.data$CodeUnits %in% c("mmol/l", "mmol/L"))),
                      NA,
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=2&sir.data$temp<=10,c("PatientID","temp","event.date")]

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumPotassium=mean(temp)) %>%
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

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=20&sir.data$temp<=200,c("PatientID","temp","event.date")]

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(HeartRate=mean(temp)) %>%
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
table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% BNP1.csv$ReadCode]), useNA = "ifany")
#      ng/L  None pg/mL 
# 10   828     6     1 

smalltab<-sir.data[!is.na(sir.data$temp)&sir.data$temp>=1&sir.data$temp<=1000,c("PatientID","temp","event.date")]
columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(BNP=mean(as.numeric(temp))) %>%
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
table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% NTPROBNP1.csv$ReadCode]), useNA = "ifany")
#1ng/L=1pg/ml
#     ng/L  None pg/mL 
# 1    90    23    12
smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=1&
                     sir.data$temp<=6000,
                   c("PatientID","temp","event.date")]

columns=names(smalltab[c(1,3)])

dots<-lapply(columns, as.symbol)

first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(NTPROBNP=mean(as.numeric(temp))) %>%
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
sir.data$SBP<-ifelse(sir.data$ReadCode %in% SBP1.csv$ReadCode,
                     as.numeric(paste(sir.data$CodeValue)),
                     NA)

sir.data$temp<-sir.data$SBP

table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% SBP1.csv$ReadCode]), useNA = "ifany")
#         mm hg  mm Hg   mmHg   None mm[Hg] 
# 32618    338 269970   3563    186     11 
# there is a substantial proportion with no units. Let's see if the distribution is similar to the other
droplevels(sir.data[sir.data$ReadCode %in% SBP1.csv$ReadCode, ]) %>%
  group_by(CodeUnits) %>%
    summarise(min = min(temp),
              first = quantile(temp,probs = 0.25, na.rm = TRUE),
              median = quantile(temp,probs = 0.50, na.rm = TRUE),
              third = quantile(temp,probs = 0.75, na.rm = TRUE),
              max = max(temp))
  
# # A tibble: 6 x 6
# CodeUnits     min first median third   max
# <fct>       <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1 ""           11  116     130  146  96110
# 2 mm hg        96  126     136  144.   214
# 3 mm Hg         0  125     138  150  16078
# 4 mmHg         33  118     130  140    260
# 5 None          0    0       0    0      0
# 6 mm[Hg]      108  114.    123  133    165

#it is fine, the None values and unrealistic high values will be removed by the below

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=30&
                     sir.data$temp<=300,
                   c("PatientID","SBP","event.date")] #SBP is correct this time instead of temp to then have SBP as the output variable

columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  slice(which.min(as.numeric(SBP))) %>%
  ungroup()%>%
  as.data.frame

indx2 <- neardate(crea.rep$PatientID, #why indx2?!
                  first$PatientID, 
                  crea.rep$event.date, 
                  first$event.date, 
                  best="prior")

crea.rep$SBP<-first[indx2, "SBP"]
crea.rep$SBPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx2])) >30, 1, 0)
crea.rep$SBP<-unlist(crea.rep$SBP)

summary(crea.rep)

#MIN DAILY DIASTOLIC BP #30 DAYS
sir.data$DBP<-ifelse(sir.data$ReadCode %in% DBP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
sir.data$temp<-sir.data$DBP

droplevels(sir.data[sir.data$ReadCode %in% DBP1.csv$ReadCode, ]) %>%
  group_by(CodeUnits) %>%
  summarise(min = min(temp),
            first = quantile(temp,probs = 0.25, na.rm = TRUE),
            median = quantile(temp,probs = 0.50, na.rm = TRUE),
            third = quantile(temp,probs = 0.75, na.rm = TRUE),
            max = max(temp))

# # A tibble: 6 x 6
# CodeUnits     min first median third   max
# <fct>       <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1 ""           13    62     70  80     950
# 2 mm hg        58    72     78  82     100
# 3 mm Hg         0    69     77  84     878
# 4 mmHg         13    66     73  80     132
# 5 None          0     0      0   0       0
# 6 mm[Hg]       63    66     73  76.5    80

#same as before

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=20&
                     sir.data$temp<=200,
                   c("PatientID","DBP","event.date")]

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
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode]))

#        g/l    g/L   None 
#1519    384 295825      2 

droplevels(sir.data[sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode, ]) %>%
  group_by(CodeUnits) %>%
  summarise(min = min(temp),
            first = quantile(temp,probs = 0.25, na.rm = TRUE),
            median = quantile(temp,probs = 0.50, na.rm = TRUE),
            third = quantile(temp,probs = 0.75, na.rm = TRUE),
            max = max(temp))

# # A tibble: 4 x 6
# CodeUnits   min first median third   max
# <fct>       <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1 ""         0       41     43    45   51 
# 2 g/l        1.13    40     42    44  159.
# 3 g/L        0.63    38     42    44   78 
# 4 None       0        0      0     0    0 

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=10&
                     sir.data$temp<=60,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumAlbumin=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID,
                 first$PatientID,
                 crea.rep$event.date,
                 first$event.date, 
                 best="prior")

crea.rep$SerumAlbumin<-first[indx1, "SerumAlbumin"]
crea.rep$SerumAlbuminDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - firstU$event.date[indx1])) >365, 1, 0)
crea.rep$SerumAlbumin<-unlist(crea.rep$SerumAlbumin)

summary(crea.rep)

#MAX DAILY URINE ALBUMIN 1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% UAlbumin1.csv$ReadCode]))
#           g     g/L    mg/l    mg/L mg/mmol    None 
# 175       1      52      35   19157       7     111 

# convert g/l
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode&
                        sir.data$CodeUnits=="g/L",
                      sir.data$temp*1000,
                      sir.data$temp)
# all the rest NAs
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode&
                        (sir.data$CodeUnits %in% c("g/L", "mg/l","mg/L")),
                      sir.data$temp,
                      NA)

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=0&
                     sir.data$temp<=1000,
                   c("PatientID","temp","event.date")]

columns=names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UrineAlbumin=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, 
                 first$PatientID, 
                 crea.rep$event.date, 
                 first$event.date, 
                 best="prior")

crea.rep$UrineAlbumin<-first[indx1, "UrineAlbumin"]
crea.rep$UrineAlbuminDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$UrineAlbumin<-unlist(crea.rep$UrineAlbumin)

summary(crea.rep)

#MEAN DAILY URINE ALBUMIN CREATININE RATIO #1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UACR1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

table(droplevels(sir.data$CodeUnits[sir.data$ReadCode %in% UACR1.csv$ReadCode]))

droplevels(sir.data[sir.data$ReadCode %in% UACR1.csv$ReadCode, ]) %>%
  group_by(CodeUnits) %>%
  summarise(min = min(temp),
            first = quantile(temp,probs = 0.25, na.rm = TRUE),
            median = quantile(temp,probs = 0.50, na.rm = TRUE),
            third = quantile(temp,probs = 0.75, na.rm = TRUE),
            max = max(temp),
            N = n())

# # A tibble: 10 x 7
# CodeUnits        min first median  third     max     N
# <fct>           <dbl> <dbl>  <dbl>  <dbl>   <dbl> <int>
# 1 ""              0    0       0      1.33 7789      577
# 2 0.2             0    0       0      0       0        1
# 3 g/mol           0    0.82    1.98   7.25 9850    33438
# 4 mg/mmol         0    0.76    1.78   5.05  905.    2888
# 5 mmol/L          4.4  7.7   104.   200     200        4
# 6 None            0    0       0      0       0     1823
# 7 ratio           0.1  0.932   2.14   7.16   81.9     88
# 8 mg/mmol(creat)  0.15 0.730   1.49   2.16    3.78     6
# 9 GPL U/ml        0    0       0      0       0        1
# 10 1.06           0    0       0      0       0        1

sir.data$temp<-ifelse(sir.data$ReadCode %in% UACR1.csv$ReadCode&
                        sir.data$CodeUnits %in% c("None", "", "1.06", "0.2", "mmol/L","GPL U/ml"),
                      NA,
                      sir.data$temp)

smalltab <- sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=0&
                     sir.data$temp<=3000,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])

dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(UACratio=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, 
                 first$PatientID, 
                 crea.rep$event.date, 
                 first$event.date, 
                 best="prior")

crea.rep$UACratio<-first[indx1, "UACratio"]
crea.rep$UACDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$UACratio<-unlist(crea.rep$UACratio)

summary(crea.rep)

#MAX DAILY MEAN CORPUSCULAR VOLUME #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% MCV1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

droplevels(sir.data[sir.data$ReadCode %in% MCV1.csv$ReadCode, ]) %>%
  group_by(CodeUnits) %>%
    summarise(min = min(temp),
              first = quantile(temp,probs = 0.25, na.rm = TRUE),
              median = quantile(temp,probs = 0.50, na.rm = TRUE),
              third = quantile(temp,probs = 0.75, na.rm = TRUE),
              max = max(temp),
              N = n()) %>%
      arrange(desc(N))

# # A tibble: 27 x 7
# CodeUnits   min first median third   max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 fL         0.8   86.6   90.6  94.7 146.  167305
# 2 fl         0.41  86.5   90.3  94   148.  143251
# 3 ""         0     86.9   90.6  93.6 120     1324
# 4 microns   68.2   85.5   90    94.6 109.     316
# 5 *fl       74.5   87.7   92.1  94.8 122.      85
# 6 None       0      0      0     0     0       32
# 7 85.2       0      0      0     0     0        2
# 8 100        0      0      0     0     0        2
# 9 88.2       0      0      0     0     0        2
# 10 pg        24.4   24.4   24.4  24.4  24.4      1
# # ... with 17 more rows

#let's keep only the data with units in the first five rows -> same distribution and consistent numbers

sir.data$temp<-ifelse(sir.data$ReadCode %in% MCV1.csv$ReadCode&
                        sir.data$CodeUnits %in% c("fL", "fl", "", "microns", "*fl"),
                      sir.data$temp,
                      NA)

#1femtoliter=1 cubic micron

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=50&
                     sir.data$temp<=150,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(MCV=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID,
                 first$PatientID,
                 crea.rep$event.date,
                 first$event.date, 
                 best="prior")

crea.rep$MCV<-first[indx1, "MCV"]
crea.rep$MCVDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >120, 1, 0)
crea.rep$MCV<-unlist(crea.rep$MCV)

summary(crea.rep)

#MAX DAILY HAEMOGLOBIN #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

droplevels(sir.data[sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode, ])%>%
  group_by(CodeUnits) %>%
    summarise(min = min(temp),
              first = quantile(temp,probs = 0.25, na.rm = TRUE),
              median = quantile(temp,probs = 0.50, na.rm = TRUE),
              third = quantile(temp,probs = 0.75, na.rm = TRUE),
              max = max(temp),
              N = n()) %>%
      arrange(desc(N))

# # A tibble: 37 x 7
# CodeUnits   min first median third        max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl>      <dbl>  <int>
# 1 g/L        5.2  108    122   136          231 162227
# 2 g/l        0    108    124   137          232 142942
# 3 mmol/mol   0     42     49    61          178  30558
# 4 None       0      0      0     0            0   7389
# 5 g/dL       0     12.3   13.7  16          219   6596
# 6 ""         0      0      0   102.         205   3860
# 7 g/dl       0.81  12.1   13.3  15.5        178    893
# 8 *g/L       1.28  13.1   14.6 111          163    110
# 9 %          5.3    6.4    6.9   8.4 3720370000     43
# 10 150        0      0      0     0            0      3
# # ... with 27 more rows

#let's convert everything to g/l

sir.data$temp<-ifelse(!is.na(sir.data$temp)&
                        sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode &
                        (sir.data$CodeUnits=="g/dl"|sir.data$CodeUnits=="g/dL")& 
                        sir.data$temp < 25,
                      sir.data$temp*10,
                      sir.data$temp)

# let's explore th records with mmol/mol

sir.data %>% 
  filter(CodeUnits == "mmol/mol" & ReadCode %in% Haemoglobin1.csv$ReadCode) %>%
    group_by(ReadCode, Rubric) %>%
      count() %>%
        arrange(desc(n))

# # A tibble: 4 x 3
# # Groups:   ReadCode, Rubric [4]
# ReadCode Rubric                                        n
# <fct>    <fct>                                     <int>
# 1 42W5.    Haemoglobin A1c level - IFCC standardised 22357
# 2 42W5.    HbA1c (IFCC)                               7979
# 3 42W5.    HbA1c levl - IFCC standardised              221
# 4 42W5.    ""                                            1

# this is HbA1c data not Hb. We will get rid of these records by not including mmol/mol in the below ifelse

sir.data$temp<-ifelse(is.na(sir.data$temp)&
                        sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode&
                        !(sir.data$CodeUnits %in% c("g/L", "g/l", "g/dL", "g/dl")),
                      NA,
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=30&
                     sir.data$temp<=260,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])

dots<-lapply(columns, as.symbol)

first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(Haemoglobin=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, 
                 first$PatientID, 
                 crea.rep$event.date, 
                 first$event.date, 
                 best="prior")

crea.rep$Haemoglobin<-first[indx1, "Haemoglobin"]
crea.rep$HaemDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >120, 1, 0)
crea.rep$Haemoglobin<-unlist(crea.rep$Haemoglobin)
#save(crea.rep, file = "crea.repongoing.rda")
summary(crea.rep)

save(crea.rep, file = "SIR_crea.repongoing.rda")

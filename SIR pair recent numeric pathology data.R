#memory.size(70000) #Assign sufficient memory to R to accomplish tasks required

library("zoo")
#library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("tidyverse")
library("zoo")
library("data.table")
library("survival")
library("lubridate")

summarise_units_distribution <- function(d, read_codes){
  
  d %>%
    filter(ReadCode %in% read_codes) %>%
      droplevels() %>%
        group_by(CodeUnits) %>%
          summarise(min = min(temp),
                    first = quantile(temp,probs = 0.25, na.rm = TRUE),
                    median = quantile(temp,probs = 0.50, na.rm = TRUE),
                    third = quantile(temp,probs = 0.75, na.rm = TRUE),
                    max = max(temp),
                    N = n()) %>%
            arrange(desc(N)) %>%
              print(n = 100)
  
}

#######################################################

load("SIR_crea.rep2yrsall.rda") #primary table
load("sir.data2yrsall.rda") #full table containing all patient data

sir.data <- sir.data[sir.data$PatientID %in% crea.rep$PatientID,] # keep only patients that respect incl criteria
sir.data$CodeValue <- as.numeric(as.character(sir.data$CodeValue)) # convert CodeValue to numeric
sir.data <- sir.data[!is.na(sir.data$CodeValue),] # remove NAs
sir.data <- sir.data %>% 
  filter(!(event.date<as.Date("1900-01-01") | event.date>as.Date("2017-08-01"))) # remove records with impossible dates / does it make sense to keep records until 1900?

summary(sir.data$event.date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1905-12-28" "2006-08-14" "2010-03-23" "2009-07-27" "2013-04-09" "2017-07-31" 

#######################################################
#Suffix files with a shared symbol to load them simultaneously:
allfiles = list.files(pattern="*1.csv")
for (i in 1:length(allfiles)) assign(allfiles[i], read.csv(allfiles[i]))
#######################################################

#CODE CONDITIONS REQUIRING NUMERIC VALUES TO BE ADDED, NEAREST IF WITHIN X TIME PERIOD
#NOTE THAT COERCION OF TIBBLES TO DATAFRAME FORMAT MAY NOT BE MAINTAINED IF YOU SAVE R OBJECTS
#TIBBLE FORMATTING CAUSING ISSUES CAN ALSO BE CLEARED BY SAVING MIDWAY POINTS AS CSV FILES

#NEAREST PRIOR MEAN DAILY SERUM SODIUM #DATE FLAG 30 DAYS
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumSodium1.csv$ReadCode, 
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

summarise_units_distribution(d = sir.data, read_codes = SerumSodium1.csv$ReadCode)
# # A tibble: 12 x 7
# CodeUnits   min first median third   max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 mmol/L      0     137    140   142   179 421304
# 2 mmol/l      4.3   138    140   142   152   2053
# 3 ""        125     139    141   142  1422   1783
# 4 None        0       0      0     0     0      8
# 5 %         144     144    144   144   144      1
# 6 1         140     140    140   140   140      1
# 7 mmol      138     138    138   138   138      1
# 8 127         0       0      0     0     0      1
# 9 134         0       0      0     0     0      1
# 10 138         0       0      0     0     0      1
# 11 132         0       0      0     0     0      1
# 12 136         0       0      0     0     0      1

# first three have the most numbers and same distribution let's keep them

sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumSodium1.csv$ReadCode &
                        sir.data$CodeUnits %in% c("mmol/L", "mmol/l", "") &
                        sir.data$temp>=20 & 
                        sir.data$temp<=3000, 
                      sir.data$temp,
                      NA)

smalltab<-sir.data[!is.na(sir.data$temp),
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])

dots<-lapply(columns, as.symbol)

first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumSodium=mean(temp)) %>% 
  as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                 best="prior")
crea.rep$SerumSodium<-first[indx1, "SerumSodium"]
crea.rep$SerSodDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)

summary(crea.rep$SerumSodium)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 44.0   137.0   140.0   139.1   142.0  1422.0     618 

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

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#        0        2        2        2        2        2 19363376 

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

sir.data$weight<-ifelse(sir.data$ReadCode %in% Weight1.csv$ReadCode & 
                        !sir.data$CodeUnits=="kg/m2",
                        sir.data$CodeValue,
                        NA)


sir.data$temp<-ifelse(is.na(sir.data$temp) & !is.na(sir.data$weight) & !is.na(sir.data$Height), # if bmi not found as a record
                      sir.data$weight/sir.data$Height^2, # calculate bmi with height and weight
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

summary(crea.rep$BMI)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 10.60   24.11   27.80   28.66   32.30   68.69   18265 

#MEAN DAILY SERUM URIC ACID #1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UricAcid1.csv$ReadCode, 
                      sir.data$CodeValue,
                      NA)

summarise_units_distribution(sir.data, UricAcid1.csv$ReadCode)

# # A tibble: 8 x 7
#   CodeUnits   min   first median   third    max     N
#   <fct>     <dbl>   <dbl>  <dbl>   <dbl>  <dbl> <int>
# 1 mmol/L     0.08   0.34    0.44   0.53    1.14  8871
# 2 umol/L     0.54 342     429    538     913      261
# 3 ""         0      0       0      0.385 463      119
# 4 None       0      0       0      0       0       79
# 5 IU/L       0.25   0.42    0.52  22.9   697       42
# 6 iu/L       0.26   0.5     0.55   0.65  760       13
# 7 umol/l     0.42   0.448   0.5    0.56   48        6
# 8 mmol       0.38   0.38    0.38   0.38    0.38     1

#let's convert umol/l to mmol/l and disregard the rest

sir.data$temp<-ifelse((sir.data$CodeUnits=="umol/L"|sir.data$CodeUnits=="umol/l") & 
                        sir.data$ReadCode %in% UricAcid1.csv$ReadCode,
                      sir.data$temp/1000,
                      sir.data$temp)

sir.data$temp<-ifelse(!(sir.data$CodeUnits %in% c("mmol/L", "umol/L", "umol/l")) &
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
summary(crea.rep$UricAcid)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.08    0.31    0.39    0.41    0.49    1.00  202642

#MEAN DAILY BLOOD UREA NITROGEN #30 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode,
                      sir.data$CodeValue,
                      NA)


summarise_units_distribution(sir.data, BUN1.csv$ReadCode)
# A tibble: 49 x 7
# CodeUnits   min   first median  third    max      N
# <fct>     <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <int>
#   1 mmol/L     0      5.4     7.2   10.4  234    428170
# 2 ""         0      4.4     5.8    7.3  463      1958
# 3 mmol/l     1.6    5.1     6.7    8.9  124      1450
# 4 None       0      0       0      0      0       277
# 5 umol/L     0.54 343.    426    545    913       192
# 6 MMOL/L     2.7    5.3     6.4   11.6   31        84
# 7 IU/L       0.25   0.42    0.52  22.9  697        42
# 8 iu/L       0.26   0.5     0.55   0.65 760        13
# 9 umol/l     0.42   0.448   0.5    0.56  48         6
# 10 4.2        0      0       0      0      0         3
# 11 6.8        0      0       0      0      0         3
# 12 4.6        0      0       0      0      0         3
# 13 5.6        0      0       0      0      0         2
# 14 6.5        0      0       0      0      0         2
# 15 6          0      0       0      0      0         2
# 16 4.5        0      0       0      0      0         2
# 17 4.4        0      0       0      0      0         2
# 18 7          0      0       0      0      0         2
# 19 %          6.7    6.7     6.7    6.7    6.7       1
# 20 6.6        0      0       0      0      0         1
# 21 6.9        0      0       0      0      0         1
# 22 7.4        0      0       0      0      0         1
# 23 mmol       0.38   0.38    0.38   0.38   0.38      1
# 24 3.6        0      0       0      0      0         1
# 25 h         12.1   12.1    12.1   12.1   12.1       1
# 26 5          0      0       0      0      0         1
# 27 4.9        0      0       0      0      0         1
# 28 10.2       0      0       0      0      0         1
# 29 7.6        0      0       0      0      0         1
# 30 3.1        0      0       0      0      0         1
# 31 5.3        0      0       0      0      0         1
# 32 6.1        0      0       0      0      0         1
# 33 7.2        0      0       0      0      0         1
# 34 11.8       0      0       0      0      0         1
# 35 4.7        0      0       0      0      0         1
# 36 7.1        0      0       0      0      0         1
# 37 8.7        0      0       0      0      0         1
# 38 23.3       0      0       0      0      0         1
# 39 3.8        0      0       0      0      0         1
# 40 3.7        0      0       0      0      0         1
# 41 5.5        0      0       0      0      0         1
# 42 9.4        0      0       0      0      0         1
# 43 8.3        0      0       0      0      0         1
# 44 5.9        0      0       0      0      0         1
# 45 10.9       0      0       0      0      0         1
# 46 11.3       0      0       0      0      0         1
# 47 16.8       0      0       0      0      0         1
# 48 7.3        0      0       0      0      0         1
# 49 12.1       0      0       0      0      0         1


# there are values in the units column let's put them in temp if temp is na (e.g. there is no result)



sir.data$temp<-ifelse(sir.data$ReadCode %in% BUN1.csv$ReadCode & 
                        as.numeric(as.character(sir.data$CodeUnits))>=1 & 
                        (!is.na(sir.data$temp) | sir.data$temp == 0),
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

summary(crea.rep$BUN)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    3.10    4.40    6.00    6.84    7.00   23.30  299952 

#MEAN DAILY SERUM POTASSIUM #1 MONTH DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode,
                      sir.data$CodeValue,
                      NA)
summarise_units_distribution(sir.data, SerumPotassium1.csv$ReadCode)

# # A tibble: 11 x 7
# CodeUnits   min first median third   max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
#   1 mmol/L     0.77  4      4.4   4.8  405   420442
# 2 mmol/l     0     4.2    4.5   4.9  309     1967
# 3 ""         0     4      4.4   4.7  309     1953
# 4 None       0     0      0     0      0      182
# 5 mmoL/L     3.4   3.95   4.1   4.20   4.3      7
# 6 3.6        0     0      0     0      0        2
# 7 mmol/L%    3.3   3.52   3.75  3.98   4.2      2
# 8 %          3.9   3.9    3.9   3.9    3.9      1
# 9 5.6        0     0      0     0      0        1
# 10 mL/min     3.8   3.8    3.8   3.8    3.8      1
# 11 4.5        0     0      0     0      0        1

#The distribution is the same for mmol/l and the empty string, let's keep those put NAs for the others

sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumPotassium1.csv$ReadCode &
                        (!(sir.data$CodeUnits %in% c("mmol/l", "mmol/L", "", "mmoL/L"))),
                      NA,
                      sir.data$temp)

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=2&
                     sir.data$temp<=10,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(SerumPotassium=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID,
                 first$PatientID,
                 crea.rep$event.date,
                 first$event.date, 
                 best="prior")

crea.rep$SerPotassium<-first[indx1, "SerumPotassium"]
crea.rep$SerPotDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)

summary(crea.rep$SerPotassium)
# Min.    1st Qu.  Median Mean   3rd Qu.    Max.    NA's 
# 2.000   4.000   4.400   4.389   4.700  10.000     783 


#MEAN DAILY HEART RATE #30 DAYS
sir.data$temp<-ifelse(sir.data$ReadCode %in% HeartRate1.csv$ReadCode,
                      sir.data$CodeValue,
                      NA)

summarise_units_distribution(sir.data, HeartRate1.csv$ReadCode)
# # A tibble: 8 x 7
#   CodeUnits      min first median third   max     N
#   <fct>        <dbl> <dbl>  <dbl> <dbl> <dbl> <int>
# 1 /minute          5  64       73  82     150  5275
# 2 beats/min       34  63       72  81     160  1513
# 3 Beats/min       42  60       68  79.8   138   622
# 4 beats/minute     0  63.5     71  80     179   579
# 5 ""              48  60       64  76    2472    50
# 6 /min            58  68       76  80     120    17
# 7 None             0   0        0   0       0    12
# 8 bpm             80  80       80  80      80     1
#units are fine, incorrect values will be removed by the below filter

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=20&
                     sir.data$temp<=200,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(HeartRate=mean(temp)) %>%
  as.data.frame

indx1 <-neardate(crea.rep$PatientID,
                 first$PatientID,
                 crea.rep$event.date,
                 first$event.date, 
                 best="prior")

crea.rep$HeartRate<-first[indx1, "HeartRate"]
crea.rep$HeartRateDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
crea.rep$HeartRateDateFlag<-ifelse(is.na(crea.rep$HeartRateDateFlag),0,crea.rep$HeartRateDateFlag)
crea.rep$HeartRate_DF<-ifelse(crea.rep$HeartRateDateFlag==0,crea.rep$HeartRate,NA)

summary(crea.rep$HeartRate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 34.00   64.00   74.00   75.24   84.00  179.00  269997

#MEAN BNP #1 YEAR
sir.data$temp<-ifelse(sir.data$ReadCode %in% BNP1.csv$ReadCode,
                     sir.data$CodeValue,
                     NA)
#1ng/L=1pg/ml
summarise_units_distribution(sir.data, BNP1.csv$ReadCode)
# # A tibble: 4 x 7
#   CodeUnits   min first median third   max     N
#   <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl> <int>
# 1 ng/L         12  283     831 2181  36520   829
# 2 ""           78  695.   1486 2634.  7397    10
# 3 None          0    0       0    0      0     6
# 4 pg/mL      3119 3119    3119 3119   3119     1

#units are fine, zeros removed by the below filter

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=1&
                     sir.data$temp<=1000,
                   c("PatientID","temp","event.date")]

columns <- names(smalltab[c(1,3)])

dots<-lapply(columns, as.symbol)
first <-smalltab %>% 
  group_by_(.dots=dots) %>%
  summarise(BNP=mean(as.numeric(temp))) %>%
  as.data.frame 

indx1 <-neardate(crea.rep$PatientID,
                 first$PatientID,
                 crea.rep$event.date,
                 first$event.date, 
                 best="prior")

crea.rep$BNP<-first[indx1, "BNP"]

crea.rep$BNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$BNPDateFlag<-ifelse(is.na(crea.rep$BNPDateFlag),0,crea.rep$BNPDateFlag)
crea.rep$BNP_DF<-ifelse(crea.rep$BNPDateFlag==0,crea.rep$BNP,NA)

summary(crea.rep$BNP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 12.0   148.0   374.0   409.4   627.0   986.0  296071 

#MEAN NT-PRO BNP #1 YEAR
sir.data$temp<-ifelse(sir.data$ReadCode %in% NTPROBNP1.csv$ReadCode,
                      sir.data$CodeValue,
                      NA)

summarise_units_distribution(sir.data, NTPROBNP1.csv$ReadCode)
# # A tibble: 4 x 7
# CodeUnits   min first median third   max     N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl> <int>
# 1 ng/L         15 366     982. 1993  25976    90
# 2 None          0   0       0     0      0    23
# 3 pg/mL        56  84.2  1271  2481.  7092    12
# 4 ""          131 131     131   131    131     1# 

#units are fine
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

indx1 <-neardate(crea.rep$PatientID, 
                 first$PatientID, 
                 crea.rep$event.date, 
                 first$event.date, 
                 best="prior")

crea.rep$NTPROBNP<-first[indx1, "NTPROBNP"]

crea.rep$NTPROBNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
crea.rep$NTPROBNPDateFlag<-ifelse(is.na(crea.rep$NTPROBNPDateFlag),0,crea.rep$NTPROBNPDateFlag)
crea.rep$NTPROBNP_DF<-ifelse(crea.rep$NTPROBNPDateFlag==0,crea.rep$NTPROBNP,NA)

summary(crea.rep$NTPROBNP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 15     241     948    1502    1877    4875  301394

#MIN DAILY SYSTOLIC BP #30 DAYS
sir.data$SBP<-ifelse(sir.data$ReadCode %in% SBP1.csv$ReadCode,
                     as.numeric(paste(sir.data$CodeValue)),
                     NA)

sir.data$temp<-sir.data$SBP

summarise_units_distribution(sir.data, SBP1.csv$ReadCode)
# A tibble: 6 x 7
# CodeUnits   min first median third   max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 mm Hg         0  125     138  150  16078 269861
# 2 ""           11  116     130  146  96110  32598
# 3 mmHg         33  118     130  140    260   3563
# 4 mm hg        96  126     136  144.   214    338
# 5 None          0    0       0    0      0    186
# 6 mm[Hg]      108  114.    123  133    165     11
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

summary(crea.rep$SBP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 33.0   120.0   130.0   131.4   142.0   276.0    9339 

#MIN DAILY DIASTOLIC BP #30 DAYS
sir.data$DBP<-ifelse(sir.data$ReadCode %in% DBP1.csv$ReadCode,as.numeric(paste(sir.data$CodeValue)),NA)
sir.data$temp<-sir.data$DBP

summarise_units_distribution(sir.data, DBP1.csv$ReadCode)

# # A tibble: 6 x 7
#   CodeUnits   min first median third   max      N
#   <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 mm Hg         0    69     77  84     878 268597
# 2 ""           13    62     70  80     950  32454
# 3 None          0     0      0   0       0  24656
# 4 mmHg         13    66     73  80     132   3333
# 5 mm hg        58    72     78  82     100    286
# 6 mm[Hg]       63    66     73  76.5    80     11

#same as before

smalltab<-sir.data[!is.na(sir.data$temp)&
                     sir.data$temp>=20&
                     sir.data$temp<=200,
                   c("PatientID","DBP","event.date")]

columns <- names(smalltab[c(1,3)])
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

summary(crea.rep$DBP)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 22.00   64.00   72.00   72.59   80.00  180.00    9838 

#MEAN DAILY SERUM ALBUMIN #`1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% SerumAlbumin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

summarise_units_distribution(sir.data, SerumAlbumin1.csv$ReadCode)

# # A tibble: 4 x 7
#   CodeUnits   min first median third   max      N
#   <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 g/L        0.63    38     42    44   78  295825
# 2 ""         0       41     43    45   51    1519
# 3 g/l        1.13    40     42    44  159.    384
# 4 None       0        0      0     0    0       2

#it is fine, similar distributions throughout

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

summary(crea.rep$SerumAlbumin)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 10.00   37.00   41.00   39.58   44.00   60.00    3189 

#MAX DAILY URINE ALBUMIN 1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UAlbumin1.csv$ReadCode,
                      sir.data$CodeValue,
                      NA)

summarise_units_distribution(sir.data, UAlbumin1.csv$ReadCode)

# # A tibble: 7 x 7
# CodeUnits   min first median third    max     N
# <fct>     <dbl> <dbl>  <dbl> <dbl>  <dbl> <int>
#   1 mg/L          0 4.4     10.8 42    7271   19157
# 2 ""            0 0        4.8 12.2  3216     175
# 3 None          0 0        0    0       0     111
# 4 g/L           0 0.47     1.9  6.04  187      52
# 5 mg/l          0 0        0    0     131.     32
# 6 mg/mmol       0 0.475    1   20.6    73.5     7
# 7 g             5 5        5    5       5       1

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

columns <- names(smalltab[c(1,3)])
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

summary(crea.rep$UrineAlbumin)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    4.70   12.50   74.12   53.00 1000.00  176238

#MEAN DAILY URINE ALBUMIN CREATININE RATIO #1 YEAR DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% UACR1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

summarise_units_distribution(sir.data, UACR1.csv$ReadCode)
# # A tibble: 10 x 7
# CodeUnits        min first median  third     max     N
# <fct>          <dbl> <dbl>  <dbl>  <dbl>   <dbl> <int>
#   1 g/mol           0    0.82    1.98   7.23 9850    33444
# 2 mg/mmol         0    0.75    1.78   5.03  905.    2876
# 3 None            0    0       0      0       0     1823
# 4 ""              0    0       0      1.33 7789      577
# 5 ratio           0.1  0.772   2.05   5.79   81.9     92
# 6 mg/mmol(creat)  0.15 0.730   1.49   2.16    3.78     6
# 7 mmol/L          4.4  7.7   104.   200     200        4
# 8 0.2             0    0       0      0       0        1
# 9 GPL U/ml        0    0       0      0       0        1
# 10 1.06            0    0       0      0       0        1
# let's only keep g/mol, mg/mmol and ratio

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

summary(crea.rep$UACratio)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.88    2.13   19.29    9.08 1193.00  164724 

#MAX DAILY MEAN CORPUSCULAR VOLUME #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% MCV1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

summarise_units_distribution(sir.data, MCV1.csv$ReadCode)

# # A tibble: 27 x 7
# CodeUnits   min first median third   max      N
# <fct>     <dbl> <dbl>  <dbl> <dbl> <dbl>  <int>
# 1 fL         0.8   86.6   90.6  94.7 146.  167206
# 2 fl         0.41  86.5   90.3  94   148.  142880
# 3 ""         0     86.9   90.6  93.6 120     1324
# 4 microns   68.2   85.5   89.9  94.6 109.     313
# 5 *fl       74.5   87.8   92.1  94.8 122.      86
# 6 None       0      0      0     0     0       32
# 7 85.2       0      0      0     0     0        2
# 8 100        0      0      0     0     0        2
# 9 88.2       0      0      0     0     0        2
# 10 pg        24.4   24.4   24.4  24.4  24.4      1
# 11 93         0      0      0     0     0        1
# 12 97.4       0      0      0     0     0        1
# 13 89.1       0      0      0     0     0        1
# 14 96.9       0      0      0     0     0        1
# 15 94.2       0      0      0     0     0        1
# 16 100.1      0      0      0     0     0        1
# 17 92.8       0      0      0     0     0        1
# 18 88.8       0      0      0     0     0        1
# 19 100.6      0      0      0     0     0        1
# 20 102.9      0      0      0     0     0        1
# 21 98.9       0      0      0     0     0        1
# 22 99.5       0      0      0     0     0        1
# 23 93.8       0      0      0     0     0        1
# 24 89.4       0      0      0     0     0        1
# 25 89.9       0      0      0     0     0        1
# 26 108.9      0      0      0     0     0        1
# 27 87.3       0      0      0     0     0        1

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

summary(crea.rep$MCV)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   50.90   86.50   90.30   90.29   94.10  144.70    5646

#MAX DAILY HAEMOGLOBIN #120 DAY DATE FLAG
sir.data$temp<-ifelse(sir.data$ReadCode %in% Haemoglobin1.csv$ReadCode,
                      as.numeric(paste(sir.data$CodeValue)),
                      NA)

summarise_units_distribution(sir.data, Haemoglobin1.csv$ReadCode)

# # A tibble: 37 x 7
# CodeUnits    min first median third        max      N
# <fct>      <dbl> <dbl>  <dbl> <dbl>      <dbl>  <int>
#   1 g/L         5.2  108    122   136          231 162131
# 2 g/l         0    109    124   137          232 142576
# 3 mmol/mol    0     42     49    61          178  30576
# 4 None        0      0      0     0            0   7389
# 5 g/dL        0     12.3   13.7  16          219   6585
# 6 ""          0      0      0   102.         205   3860
# 7 g/dl        0.81  12.1   13.3  15.5        178    890
# 8 *g/L        1.28  13.1   14.7 111.         163    112
# 9 %           5.3    6.4    6.9   8.4 3720370000     43
# 10 150         0      0      0     0            0      3
# 11 mg/L       11.6   43.2   74.8 106.         138      2
# 12 mmol/L    130    130    130   130          130      2
# 13 mmol/mmol  60     78     96   114          132      2
# 14 146         0      0      0     0            0      2
# 15 124         0      0      0     0            0      2
# 16 135         0      0      0     0            0      2
# 17 148         0      0      0     0            0      2
# 18 137         0      0      0     0            0      2
# 19 131         0      0      0     0            0      2
# 20 141         0      0      0     0            0      2
# 21 %Hb       145    145    145   145          145      1

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

summary(crea.rep$Haemoglobin)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 30.0   105.0   122.0   119.9   137.0   230.0    5534 

save(crea.rep, file = "SIR_crea.repongoing.rda")

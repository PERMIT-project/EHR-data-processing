library("zoo")
#library(plyr)
library("dplyr")
library("tidyverse")
library("data.table")
library("survival")
library("lubridate")

load("sir.data.rda") #Load lng format input file


#READ CONDITION FILES- suffix names with a common suffix to import together
temp = list.files(pattern="*1.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

#######################################################################################
#SELECT PATIENTS 18 OR ABOVE AT HEART FAILURE DIAGNOSIS 

sir.dem <- read.csv("SIR_Demographics.csv", header = TRUE) %>%
  rename(PatientID = CohortPatientId)

sir.data <- sir.data %>%
  left_join(sir.dem, by = "PatientID")

hf<-sir.data[sir.data$ReadCode %in% HeartFailure1.csv$ReadCode,] #Create subset of HF patients
length(unique(as.factor(hf$PatientID))) #7254 total heart failure patients identified with confirmed Read Codes (V2)

hf$Age<-(as.numeric(year(strptime(hf$EntryDate, format="%Y%m%d"))))-hf$BirthYear
hf$hfage<-hf$Age

smalltab<-hf[,c("PatientID","hfage","EntryDate")] %>%
  distinct() %>%
    mutate(EntryDate = as.Date(as.character(EntryDate),format="%Y%m%d")) %>%
      filter(!is.na(hfage)) %>%
        arrange(PatientID)

#smalltab$PatientID<-as.factor(smalltab$PatientID)
#smalltab<-smalltab[!is.na(smalltab$hfage),]

first<- # get first HF code
  smalltab %>% 
    group_by(PatientID) %>%
      slice(which.min(hfage)) %>% 
        ungroup() %>%
          rename(hfdate = EntryDate) 

# %>%
#             mutate(hfdate = as.Date(as.character(hfdate),format="%Y%m%d"))

head(first)

# sir.data<-merge(sir.data[sir.data$PatientID %in% hf$PatientID,],as.data.frame(first),all.x=TRUE)
# sir.data<-sir.data[sir.data$hfage>=18,]

sir.data <- 
  sir.data %>%
    right_join(first, by = "PatientID") %>%
      filter(hfage >= 18)

#length(unique(as.factor(sir.data$PatientID))) #number of patients 18 or over at first diagnosis
save(sir.data,file="sirdatahfonly.rda")
####################################################################################
#SELECT PATIENTS WITH CREATININE DATA
crea <- sir.data[sir.data$ReadCode=="44J3.",] #Based on Read Codes v2- adapt as required

crea <- crea %>% 
          distinct() %>% # remove duplicates
            droplevels()

crea$CodeValue<-as.numeric(as.character(crea$CodeValue))
crea$CodeUnits <- as.character(crea$CodeUnits)

#CHECK FOR CASES VIABLE VALUE IS ENTERED IN THE 'UNITS' COLUMN BY MISTAKE
temp<-as.numeric(as.character(crea$CodeUnits))>0 & as.numeric(as.character(crea$CodeUnits))<3000  & is.na(crea$CodeValue)
table(temp, useNA = "ifany") # no row is affected, no need to do anything
# temp
# FALSE   <NA> 
#   442671   1385 

temp<-as.numeric(as.character(crea$CodeUnits))>0 & as.numeric(as.character(crea$CodeUnits))<3000  & !is.na(crea$CodeValue)
table(temp, useNA = "ifany") 
# temp
# FALSE   TRUE   <NA> 
#   1385     50 442621 
#There are 50 rows affected let's explore the distribution of the values in codevalue

tmp <- crea[!is.na(temp) & temp == TRUE, ]
summary(tmp %>% mutate(CodeUnits = as.numeric(as.character(CodeUnits))) %>% select(CodeValue, CodeUnits))

# CodeValue   CodeUnits    
# Min.   :0   Min.   : 61.0  
# 1st Qu.:0   1st Qu.: 84.5  
# Median :0   Median : 95.0  
# Mean   :0   Mean   :103.1  
# 3rd Qu.:0   3rd Qu.:115.2  
# Max.   :0   Max.   :227.0  

#these are all 0s with the CodeUnits containing the actual information, let's put them in.
crea$CodeValue <- ifelse(!is.na(temp) & temp == TRUE, 
                         as.numeric(as.character(crea$CodeUnits)), 
                         crea$CodeValue)

crea$CodeUnits <- ifelse(!is.na(temp) & temp == TRUE, 
                         "", 
                         crea$CodeUnits)

summary(crea$CodeValue)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0.0    75.0    93.0   114.9   123.0  2346.0    1385 

# remove zeros and NAs
crea<-crea %>% 
        filter(!is.na(CodeValue) & CodeValue > 0)

length(unique(crea$PatientID)) #number of hf patients over 18 at diagnosis with creatinine data
#6970

###########################################################################
#SENSITIVITY TESTS- HOW MANY ZERO CR VALUES AND HOW MANY CR VALUES UNDER 20
lowcr<-crea[crea$CodeValue<20,] 
length(lowcr$PatientID) #How many values are present but <20
levels(as.factor(lowcr$PatientID)) #How many patients are affected


##Convert values in mmol to umol

crea$CodeUnits<-as.factor(crea$CodeUnits)
levels(crea$CodeUnits)
#[1] ""         "%"        "g/L"      "h"        "micmol/l" "mmol"     "mmol/L"   "umol/L"

levels(crea$CodeUnits)[c(1,5)]<-"umol/L"
levels(crea$CodeUnits)[c(5)]<-"mmol/L"
levels(crea$CodeUnits)[c(2:4)]<-"NA"
crea<-crea[!is.na(crea$CodeUnits),]
crea$CodeValue<-ifelse(crea$CodeUnits=="mmol/L" & as.numeric(crea$CodeValue)<50,
                       (as.numeric(crea$CodeValue)*1000),
                       as.numeric(crea$CodeValue))

crea$CodeUnits<-"umol/L"

summary(crea)

# remove values outside agreed range
crea<-crea %>% 
        filter(CodeValue >= 20) %>% 
          filter(CodeValue <= 3000)

summary(crea)

save(crea,file="SIR_crearecleaned.rda")

###############################################################################
#DATA CLEANING 

#load("sirdatahfonly.rda") #full patient records from all adult hf patients from all years
#load("SIR_crearecleaned.rda") # creatinine data table including tests from selected patients

#REMOVE SAME DAY CREATININE ENTRIES IF THE SOURCE LOCATION CODE DIFFERS.
#(OCCURS IN SIR DURING TRANSFER BETWEEN PRIMARY AND SECONDARY CARE EHF SYSTEMS)

crea <- crea[!duplicated(crea[,c("PatientID","EntryDate","CodeValue","Source")]), ]

#REMOVE DELAYED CREATININE ENTRIES FROM UP-TO 30 days APART IF THE SOURCE LOCATION CODE DIFFERS.

crea$event.date<-as.Date(as.character(crea$EntryDate),format="%Y%m%d")

year <- (as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d"))))
month <- (as.numeric(month(strptime(crea$event.date, format="%Y-%m-%d"))))
crea$EntryPeriod<-paste(month,year)

## old version
# 
crea$Source<-ifelse(crea$Source=="salfordt",paste("2"),paste("1")) #Simplify source codes to '2' for hospital, '1' for GP
# crea$Source<-ifelse(is.na(crea$Source),paste("2"),crea$Source)
# 
# crea<-crea[order(crea[,1], -(crea[,4]),(crea[,7])),]
# crea4<-crea[(duplicated(crea[,c(1,6,14])&!duplicated(crea[,7])),] #DELAYED DUPLICATES (SAME PATIENT, MONTH, VALUE)
# length(crea4$PatientID) #Only 1 of the remaining potential duplicate records outside of the same day window come from different locations
# crea<-crea[!rownames(crea) %in% rownames(crea4),]

crea <- crea %>%
  arrange(PatientID, event.date, desc(Source)) %>% # salford will be always first 
    mutate(PatientID.next = c(PatientID[-1], NA), # add columns to identify duplicated values
           event.date.next = c(event.date[-1], NA),
           CodeValue.next = c(CodeValue[-1], NA),
           Source.next = c(as.character(Source[-1]), NA)) %>% 
      mutate(diff = event.date.next - event.date,
             duplicated = (PatientID == PatientID.next) & # same patient
                          (Source != Source.next)& # different data source
                          (Source == "2") & # the oldest is salford
                          (CodeValue == CodeValue.next) & # same value
                          (diff >= 0 & diff <= 30)) # within a month

sum(crea$duplicated, na.rm = TRUE) #[1] 106389

crea <- crea %>%
  filter(!duplicated)  %>% # remove delayed duplicates
    distinct(PatientID, event.date, CodeValue,.keep_all = TRUE)  %>% # remove remaining duplicates  
      select(-PatientID.next, -event.date.next, -CodeValue.next, -Source.next, -duplicated, -diff)

summary(crea)

#SELECT MEAN DAILY CREATININE IF MULTIPLE ENTRIES AFTER REMOVING DELAYED DUPLICATES AND OUT OF RANGE VALUES
smalltab<-crea[,c("PatientID","CodeValue", "EntryDate")]

xcrea<-smalltab %>% 
  group_by(PatientID, EntryDate) %>%
    summarize(Creatinine = mean(as.numeric(as.character(CodeValue)))) %>%
      ungroup()

crea<-merge(crea,as.data.frame(xcrea),all.x=TRUE) %>%
  distinct(PatientID, EntryDate, Creatinine, .keep_all = TRUE)

summary(crea)

######################################################
#Add demographic variables from lookup tables
#LSOA and age should be added here if not already present

#ASSIGN AGE (BIRTH DATE GIVEN TO NEAREST MONTH IN SIR DATA)
crea$Age<-(as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d"))))-crea$BirthYear

#CODE ETHNICITY
ethnic.data<-read.table("ethnic.data.csv",header=TRUE,sep=",") #Call data from lookup table (see Open Source Resources repo)
ethnic.data$Category<-floor(ethnic.data$Category)
crea<-merge(crea,ethnic.data,by.x="Ethnicity",by.y="ClinCode2",all.x=TRUE, all.y=FALSE)

#old
# crea<-subset(crea, select=-c(Ethnicity,ClinCode1,EntryPeriod,Rubric))
# colnames(crea)[which(names(crea) == "Category")] <- "Ethnicity"

crea <- crea %>%
  select(-Ethnicity, -ClinCode1, -EntryPeriod, -Rubric) %>%
    rename(Ethnicity = Category)

#ADD LSOA
imd<-read.csv("IMD2010.csv") #Call data from lookup table (see Open Source Resources repo)
imd<-imd[,c("LSOA","IMD_Decile2010")]

#old
#crea<-merge(crea,imd,all.x=TRUE)
crea <- crea %>%
  left_join(imd, by = "LSOA")

#LIMIT TO PATIENTS WITH AT LEAST 2 POST 2008 CREATININE TEST VALUES

crea.tmp <- crea[as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d")))>=2008,]

ids <- crea.tmp %>%
  group_by(PatientID) %>%
    count() %>%
      filter(n >= 2)
 
crea.rep <- crea[(crea$PatientID %in% ids$PatientID),]

sir.data<-sir.data[sir.data$PatientID %in% crea.rep$PatientID,]


# # add hfdate and age to crea.rep
# crea.rep <- crea.rep %>%
#   left_join(first, by = "PatientID")

#Breakpoint
#######################################################
save(crea.rep, file = "SIR_crea.rephf2tests.Rdata")
save(sir.data, file = "sir.datahf2tests.Rdata")
#######################################################


#SELECT PATIENTS WHICH HAVE AT LEAST 2 TESTS, OPTION FOR TIME RANGE RESTRICTION

sir.data$event.date <- as.Date(as.character(sir.data$EntryDate),format="%Y%m%d") #Ensure dates are in date format
#ranges <- aggregate(as.numeric(year(strptime(sir.data$event.date, format="%Y-%m-%d"))), list(sir.data$PatientID), range)

# Error in range$range <- ranges$x[, 2] - ranges$x[, 1] : 
#   object of type 'builtin' is not subsettable

# range$range <- ranges$x[,2] - ranges$x[,1]
# range_short_ids <- ranges[(which(ranges$range<2)),1] # define exclusion range as 2 years
# crea.rep <- crea.rep[-which(crea.rep$PatientID %in% range_short_ids),]

range <- sir.data %>%
  filter(event.date >= as.Date("2008-01-01") &
           event.date <= as.Date("2017-08-01")) %>% # limit to after 2008 and remove impossible dates
    group_by(PatientID) %>%
      summarise(min.event.date = min(event.date),
                max.event.date = max(event.date)) %>% # get earliest and latest date
       ungroup() %>%
        mutate(diff = as.numeric(max.event.date - min.event.date)/365)  %>% # calculate range in years
          filter(diff >= 2)

summary(range)
# PatientID     min.event.date       max.event.date            diff      
# Min.   :    1   Min.   :2008-01-01   Min.   :2010-01-03   Min.   :2.000  
# 1st Qu.: 5195   1st Qu.:2008-01-03   1st Qu.:2013-10-08   1st Qu.:5.584  
# Median :10521   Median :2008-01-09   Median :2016-09-07   Median :8.386  
# Mean   :10529   Mean   :2008-02-19   Mean   :2015-03-20   Mean   :7.085  
# 3rd Qu.:15784   3rd Qu.:2008-01-23   3rd Qu.:2016-09-28   3rd Qu.:8.715  
# Max.   :21246   Max.   :2014-08-18   Max.   :2017-07-31   Max.   :9.551 


crea.rep <- crea.rep %>% # remove patients with at least two years follow up
  filter(PatientID %in% range$PatientID)

sir.data <- sir.data %>% # remove patients with at least two years follow up
  filter(PatientID %in% range$PatientID)

save(crea.rep,file="SIR_crea.rep2yrsall.rda")
save(sir.data,file="sir.data2yrsall.rda")


# # the below introduces duplicates, I am not sure why this was done as well. To look at which practice patients are currently registered to we should look at the latest record

# nrow(crea.rep)
# #[1] 302531

# #ADD ON PRACTISE INFO TO CREA.REP
# prac<-read.csv("PractiseKeySIR.csv")
# pats<-merge(sir.data,prac,all.x=TRUE)
# pats<-pats[!is.na(pats$Source),]
# pats<-pats[!pats$Source=="",]
# pats1<-pats[pats$Source=="salfordt",]
# pats1<-pats1[,c(1,2,4)]
# pats2<-pats[!pats$Source=="salfordt",]
# pats2<-pats2[!duplicated(pats2[,c(2,4)],fromLast=TRUE),]
# pats2<-pats2[,c(1,2,4,5,7)]
# colnames(pats2)[1]<-"Source2"
# pats3<-merge(pats1,pats2,all=TRUE)
# pats3$Source2<-ifelse(is.na(pats3$Source2),paste(pats3$Source),paste(pats3$Source2))
# pats3<-pats3[,c(1,2,4:6)]
# colnames(pats3)[3]<-"Source"
# pats3<-unique(pats3)
# table(pats3$Source)
# length(pats3$Source)
# crea.rep<-merge(crea.rep,pats3,all.x=TRUE)
# 
# #Add a random practise ID for splitting the data
# Source<-unique(crea.rep$Source[!crea.rep$Source=="salfordt"])
# Source<-Source[c(1:38,40:57)] #remove NA row (hospital)
# s1<-as.data.frame(Source)
# randompracID<-sample(1:56, 56)
# s2<-as.data.frame(randompracID)
# s3<-cbind(s1,s2)
# crea.rep<-merge(crea.rep,s3,all.x=TRUE)
# crea.rep$randompracID<-ifelse(crea.rep$Source=="salfordt",0,crea.rep$randompracID)
# 
# nrow(crea.rep)
# # [1] 302674
# 


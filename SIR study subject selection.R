memory.size(100000) #Assign sufficient memory to R
load("sir.data.rda") #Load lng format input file

library(zoo)
#library(plyr)
library(tidyverse)
library(zoo)
library(data.table)
library(survival)
library(lubridate)

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
smalltab<-hf[,c("PatientID","hfage")]
#smalltab$PatientID<-as.factor(smalltab$PatientID)
smalltab<-smalltab[!is.na(smalltab$hfage),]

first<-
  smalltab %>% 
    group_by(PatientID) %>%
      slice(which.min(hfage)) %>% 
        ungroup(first)

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
crea <- droplevels(crea)

#CHECK FOR CASES VIABLE VALUE IS ENTERED IN THE 'UNITS' COLUMN BY MISTAKE
temp<-ifelse(as.numeric(as.character(crea$CodeUnits))>0 & as.numeric(as.character(crea$CodeUnits))<1000  & is.na(crea$CodeValue),crea$CodeUnits,NA)
table(temp, useNA = "ifany") 

# crea$CodeValue<-ifelse(as.numeric(as.character(crea$CodeUnits))>0 & 
#                          as.numeric(as.character(crea$CodeUnits))<1000 & 
#                          is.na(crea$CodeValue),crea$CodeUnits,crea$CodeValue)
#crea$CodeUnits<-ifelse(!is.na(temp),paste(""),paste(crea$CodeUnits))
crea$CodeUnits <- as.character(crea$CodeUnits)
crea$CodeUnits<-ifelse(as.numeric(as.character(crea$CodeUnits))>0 & as.numeric(as.character(crea$CodeUnits))<1000 & is.na(crea$CodeValue),"",crea$CodeUnits)

crea$CodeValue<-as.numeric(as.character(crea$CodeValue))

summary(crea$CodeValue)

crea<-crea[!is.na(crea$CodeValue),]
length(unique(as.factor(crea$PatientID))) #number of hf patients over 18 at diagnosis with creatinine data

###########################################################################
#SENSITIVITY TESTS- HOW MANY ZERO CR VALUES AND HOW MANY CR VALUES UNDER 20
lowcr<-crea[crea$CodeValue<20 & !is.na(crea$CodeValue),] 
length(lowcr$PatientID) #How many values are present but <20
levels(as.factor(lowcr$PatientID)) #How many patients are affected
crea<-crea[crea$CodeValue>0,] #Remove rows with no numeric creatinine value

#Convert values in mmol to umol

crea$CodeUnits<-as.factor(crea$CodeUnits)
levels(crea$CodeUnits)
#[1] ""         "%"        "g/L"      "h"        "micmol/l" "mmol"     "mmol/L"   "umol/L"

levels(crea$CodeUnits)[c(1,5)]<-"umol/L"
levels(crea$CodeUnits)[c(5)]<-"mmol/L"
levels(crea$CodeUnits)[c(2:4)]<-"NA"
crea<-crea[!is.na(crea$CodeUnits),]
crea$CodeValue<-ifelse(crea$CodeUnits=="mmol/L" & as.numeric(crea$CodeValue)<50,(as.numeric(crea$CodeValue)*1000),as.numeric(crea$CodeValue))
crea$CodeUnits<-"umol/L"

crea<-crea[as.numeric(crea$CodeValue)>=20 & !is.na(crea$CodeValue),] 
#Appy upper limit here if required
save(crea,file="crearecleaned.rda")

###############################################################################
#DATA CLEANING 

#load("sirdatahfonly.rda") #full patient records from all adult hf patients from all years
#load("crearecleaned.rda") # creatinine data table including tests from selected patients

#REMOVE SAME DAY CREATININE ENTRIES IF THE SOURCE LOCATION CODE DIFFERS.
#(OCCURS IN SIR DURING TRANSFER BETWEEN PRIMARY AND SECONDARY CARE EHF SYSTEMS)

crea <- crea[!duplicated(crea[,c(1,4,6,7)]), ]

#crea<-crea[order(crea$PatientID,crea$CodeValue, rev(crea$Source)),]
# crea2<-crea[(duplicated(crea[,c(1,4,6)])&!duplicated(crea[,7])),] #Find duplicates of value and patient ID with different sources
# length(crea2$CodeValue)
# crea3<-crea[(duplicated(crea[,c(1,4,6)])),] #Find duplicates of value, date and patient ID
# length(crea3$CodeValue) 
# 
# crea<-crea[!rownames(crea) %in% rownames(crea2),]

#REMOVE DELAYED CREATININE ENTRIES FROM SAME CALENDAR MONTH IF THE SOURCE LOCATION CODE DIFFERS.

crea$event.date<-as.Date(as.character(crea$EntryDate),format="%Y%m%d")

year <- (as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d"))))
month <- (as.numeric(month(strptime(crea$event.date, format="%Y-%m-%d"))))
crea$EntryPeriod<-paste(month,year)

## old version
# 
# crea$Source<-ifelse(crea$Source=="salfordt",paste("2"),paste("1")) #Simplify source codes to '2' for hospital, '1' for GP
# crea$Source<-ifelse(is.na(crea$Source),paste("2"),crea$Source)
# 
# crea<-crea[order(crea[,1], -(crea[,4]),(crea[,7])),]
# crea4<-crea[(duplicated(crea[,c(1,6,14])&!duplicated(crea[,7])),] #DELAYED DUPLICATES (SAME PATIENT, MONTH, VALUE)
# length(crea4$PatientID) #Only 1 of the remaining potential duplicate records outside of the same day window come from different locations
# crea<-crea[!rownames(crea) %in% rownames(crea4),]

crea <- crea %>%
  arrange(PatientID, event.date, desc(Source)) %>%
    mutate(PatientID.next = c(PatientID[-1], NA),
           event.date.next = c(event.date[-1], NA),
           CodeValue.next = c(CodeValue[-1], NA),
           Source.next = c(as.character(Source[-1]), NA)) %>% # add columns to identify duplicated values
      mutate(diff = event.date.next - event.date,
             duplicated = (PatientID == PatientID.next) &
                          (Source != Source.next)&
                          (Source == "salfordt") &
                          (CodeValue == CodeValue.next) &
                          (diff >= 0 & diff <= 30))

sum(crea$duplicated, na.rm = TRUE) #[1] 99296

crea <- crea %>%
  filter(!duplicated)  %>% # remove delayed duplicates
    distinct(PatientID, event.date, CodeValue,.keep_all = TRUE)  %>% # remove remaining duplicates  
      select(-PatientID.next, -event.date.next, -CodeValue.next, -Source.next, -duplicated, -diff)

#SELECT MEAN DAILY CREATININE IF MULTIPLE ENTRIES AFTER REMOVING DELAYED DUPLICATES AND OUT OF RANGE VALUES
smalltab<-crea[,c("PatientID","CodeValue", "EntryDate")]
xcrea<-smalltab %>% group_by(PatientID, EntryDate) %>%
  summarize(Creatinine = mean(as.numeric(as.character(CodeValue)))) %>%
    ungroup(xcrea)
crea<-merge(crea,as.data.frame(xcrea),all.x=TRUE)

######################################################
#Add demographic variables from lookup tables
#LSOA and age should be added here if not already present

#ASSIGN AGE (BIRTH DATE GIVEN TO NEAREST MONTH IN SIR DATA)
crea$Age<-(as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d"))))-crea$BirthYear

#CODE ETHNICITY
ethnic.data<-read.table("ethnic.data.csv",header=TRUE,sep=",") #Call data from lookup table (see Open Source Resources repo)
ethnic.data$Category<-floor(ethnic.data$Category)
crea<-merge(crea,ethnic.data,by.x="Ethnicity",by.y="ClinCode2",all.x=TRUE, all.y=FALSE)
crea<-subset(crea, select=-c(Ethnicity,ClinCode1,EntryPeriod,Rubric))
colnames(crea)[which(names(crea) == "Category")] <- "Ethnicity"

#ADD LSOA
imd<-read.csv("IMD2010.csv") #Call data from lookup table (see Open Source Resources repo)
imd<-imd[,c("LSOA","IMD_Decile2010")]
crea<-merge(crea,imd,all.x=TRUE)

#LIMIT TO PATIENTS WITH AT LEAST 2 POST 2008 CREATININE TEST VALUES

crea.tmp <- crea[as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d")))>=2008,]

ids <- crea.tmp %>%
  group_by(PatientID) %>%
    count() %>%
      filter(n < 2)
 
crea.rep <- crea[!(crea$PatientID %in% ids),]

sir.data<-sir.data[sir.data$PatientID %in% crea.rep$PatientID,]

#Breakpoint
#######################################################
save(crea.rep, file = "crea.rephf2tests.Rdata")
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
  filter(event.date >= as.Date("2008-01-01")) %>% # is this to be done with only data after 2008?
  group_by(PatientID) %>%
  summarise(min.event.date = min(event.date),
            max.event.date = max(event.date)) %>% # get earliest and latest date
  ungroup() %>%
  mutate(diff = (max.event.date - min.event.date)/(365*2))  %>% # calculate range in years
  filter(diff >= 2)

crea.rep <- crea.rep %>% # remove patients with at least two years follow up
  filter(PatientID %in% range$PatientID)

sir.data <- sir.data %>% # remove patients with at least two years follow up
  filter(PatientID %in% range$PatientID)



#ADD ON PRACTISE INFO TO CREA.REP
prac<-read.csv("PractiseKeySIR.csv")
pats<-merge(sir.data,prac,all.x=TRUE)
pats<-pats[!is.na(pats$Source),]
pats<-pats[!pats$Source=="",]
pats1<-pats[pats$Source=="salfordt",]
pats1<-pats1[,c(1,2,4)]
pats2<-pats[!pats$Source=="salfordt",]
pats2<-pats2[!duplicated(pats2[,c(2,4)],fromLast=TRUE),]
pats2<-pats2[,c(1,2,4,5,7)]
colnames(pats2)[1]<-"Source2"
pats3<-merge(pats1,pats2,all=TRUE)
pats3$Source2<-ifelse(is.na(pats3$Source2),paste(pats3$Source),paste(pats3$Source2))
pats3<-pats3[,c(1,2,4:6)]
colnames(pats3)[3]<-"Source"
pats3<-unique(pats3)
table(pats3$Source)
length(pats3$Source)
crea.rep<-merge(crea.rep,pats3,all.x=TRUE)

#Add a random practise ID for splitting the data
Source<-unique(crea.rep$Source[!crea.rep$Source=="salfordt"])
Source<-Source[c(1:38,40:57)] #remove NA row (hospital)
s1<-as.data.frame(Source)
randompracID<-sample(1:56, 56)
s2<-as.data.frame(randompracID)
s3<-cbind(s1,s2)
crea.rep<-merge(crea.rep,s3,all.x=TRUE)
crea.rep$randompracID<-ifelse(crea.rep$Source=="salfordt",0,crea.rep$randompracID)

save(crea.rep,file="crea.rep2yrsall.rda")



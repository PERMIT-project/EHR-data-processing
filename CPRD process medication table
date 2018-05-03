TO PROCESS DRUG DATA FROM CPRD (SOME INTERNAL QUALITY CONTROLS ALREADY APPLIED PRIOR TO DATA RELEASE)

load("drugsCPRD.rda") #load drugs data table
load("crea.rep.rda") #load primary data table

library(stringr)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)

#REMOVE ANY DUPLICATE PRECRIPTION ENTRIES

subs<-unique(drugsCPRD[,c("patid","eventdate","TYPE"),])
crea.rep<-unique(crea.rep)
colnames(subs)[colnames(subs) == 'patid'] <- 'PatientID'  #ENSURE FIELD NAMES MATCH BETWEEN TABLES
subs$DAILY_DOSE<-subs$daily_dose*subs$DOSE_PER_TAB  #Define/calculate daily dose in mg

########################################################################## 

#CALCULATE MISSING DATA WHERE POSSIBLE BASED ON PRESENT FIELD

subs$DAILY_DOSE<-as.numeric(subs$DAILY_DOSE)
subs$DOSE_PER_TAB<-as.numeric(subs$DOSE_PER_TAB)
subs$TABLETS_PER_DAY<-as.numeric(subs$daily_dose)

subs$DAILY_DOSE<-ifelse(is.na(subs$DAILY_DOSE)&!is.na(subs$TABLETS_PER_DAY),subs$TABLETS_PER_DAY*subs$DOSE_PER_TAB,subs$DAILY_DOSE)
subs$TABLETS_PER_DAY<-ifelse(is.na(subs$TABLETS_PER_DAY)&!is.na(subs$DAILY_DOSE)&!is.na(subs$DOSE_PER_TAB),subs$DAILY_DOSE/subs$DOSE_PER_TAB,subs$TABLETS_PER_DAY)
#FIND THE MEDIAN DOSE FOR EACH DRUG AND USE THIS IF MISSING

<-subs[,c("DAILY_DOSE","TYPE")]
b<-na.omit(b)
b$DAILY_DOSE<-as.numeric(b$DAILY_DOSE)

b2<-b %>% group_by(TYPE) %>% 
summarise(MEDIAN_DOSE = median(DAILY_DOSE, na.rm = TRUE)) 
%>% as.data.frame

subs<-merge(subs,b2,all.x=TRUE)
subs$DAILY_DOSE<-ifelse(is.na(subs$DAILY_DOSE),subs$MEDIAN_DOSE,subs$DAILY_DOSE)


############################################################################

unique(subs)$FORMAT
head(subs[subs$FORMAT=="whatever the format is for liquids"]) #ALTER THIS BASED ON TABLE PROPERTIES
#YOU MAY NEED TO CONVERT VOLUME DOSAGES FOR LIQUID MEDS 
#WE ARE AIMING TO CODE FOR DOSAGE IN MG NOT MLS (MG/ML CONCENTRATION VARIES). 

subs<-unique(subs)
subs$DAILY_DOSE<-signif(subs$DAILY_DOSE,digits=2) #LIMIT TO 2 SIGNIFICANT FIGURES TO ALLOW CATEGORICAL COLUMNS

#Get start and end dates in the same format
subs$eventdate<-as.Date(subs$eventdate,format="%d/%m/%Y")
subs$END_DATE<-as.Date(subs$END_DATE,format="%d/%m/%Y")

save(subs,file="CPRDmeddata.rda")

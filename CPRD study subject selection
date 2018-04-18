require(haven)
require(gtools)
library(zoo)
library(plyr)
library(tidyverse)
library(data.table)
library(survival)
library(lubridate)
CPRD<-read_dta("hf_cases_therapy.dta") #Medications
CPRD2<-read_dta("hf_cases_clinical.dta") 
CPRD3<-read_dta("hf_cases_test.dta") 
CPRD5<-read_dta("hf_cases_additional.dta") 
#head(CPRD[CPRD$medcode=="5",]) #Only 2312 #References some dates of creatinine measures but no additional data
#crea<-CPRD3[CPRD3$medcode=="5",] #1579351 #This is the file with the creatinine values in
load("creaongoing.rda")
#this is the file of Creatinine entries to be used as a reference 
#####################################################################################################
#SELECTING FOR HEART FAILURE
#SELECT PATIENTS WHO WERE 18 OR ABOVE AT THE TIME OF HEART FAILURE DIAGNOSIS

#Add hfage for selection

CPRD4<-read_dta("hf_cases_patient_practice_incidentHFdate.dta")  
CPRD4$hfage<-(as.numeric(year(strptime(CPRD4$eventdate, format="%Y-%m-%d"))))-CPRD4$yob
CPRD4$hfage<-ifelse(as.numeric(month(strptime(CPRD4$eventdate, format="%Y-%m-%d")))>CPRD4$mob&CPRD4$mob>0,CPRD4$hfage-1,CPRD4$hfage)
CPRD4<-CPRD4[,c(1:8,18,19)]
head(CPRD4[duplicated(CPRD4$patid),]) 
#None of the records are duplicated- each gives the hfdate based on 97 hf codes

CPRD4<-CPRD4[CPRD4$hfage>=18,]
save(CPRD4,file="CPRD4.rda")
hfnames<-unique(CPRD4$patid)
length(hfnames) 
#282219

crea<-crea[crea$patid %in% hfnames,] 
CPRD4<-CPRD4[,c(1,2,6:10)]
CPRD4$hfdate<-CPRD4$eventdate
CPRD4<-CPRD4[,c(1,3:8)]
CPRD4<-as.data.frame(CPRD4)
crea<-as.data.frame(crea)
crea<-merge(crea,CPRD4,all.x=TRUE,all.y=FALSE)

save(crea, file = "crea.Rdata") 

##################################################################################################### 
#START WITH A SKELETON OF CREATININE VALUES THEN ADD ON TO IT.

#Remove outliers then order and select the highest value per day
crea<-crea[!is.na(crea$data2),]
crea<-crea[order(-crea$data2),]

smalltab<-crea %>%
group_by_(.dots=c("patid","eventdate")) %>% 
summarize(Creatinine = mean(data2)) %>%
ungroup()%>%
as.data.frame
crea<-merge(crea,as.data.frame(smalltab),all.x=TRUE)

save(crea, file = "crea2.rda") 

#######################################################################################################
#CHECK UNITS IN CASE VALUES NEED CONVERTING

#C<-unique(crea$data3) 
#Should be 142(umol/L). Lookups for units are in the text file 'SUM.txt'
#write.csv(C,file="SUMcrea.csv")
#HAND ANNOTATE WITH CONVERSION FACTORS

C<-read.csv("SUMcrea.csv")
colnames (C)[1]<-"data3"
crea<-merge(crea,C[,c(1,3,4)],all.x=TRUE,all.y=FALSE)
save(crea, file = "crea2.rda") 
#BIND ON UNITS OF MEASUREMENT

#CONVERT PROBLEMATIC UNITS
#crea$data2<-ifelse(!crea$CodeUnits=="umol/L",(crea$data2*crea$Times),crea$data2)
#MMOL/L UNITS SEEM TO BE MISNAMED THROUGHOUGH-ALL ARE IN RANGE IF UMOL/L ASSUMED 
#WE LOSE TOO MUCH DATA IF WE CONVERT THEM THEN OMIT THESE.

#CUTOFF IMPLAUSIBLE VALUES
crea<-crea[crea$data2>=20&crea$data2<=3000,]
#1422167 rows remaining

save(crea, file = "crea2.rda")
length(levels(as.factor(crea$patid)))
#120731 patients

########################################################################################################
#Add birth and death and basic formulaic variables
crea$Dead<-ifelse(!is.na(crea$deathdate),1,0)
#The death dates are not reliable, many of them are before 1950 and 100 are before 1980.

crea$Age<-(as.numeric(year(strptime(crea$eventdate, format="%Y-%m-%d"))))-crea$yob
crea$Age<-ifelse(as.numeric(month(strptime(crea$eventdate, format="%Y-%m-%d")))>crea$mob&crea$mob>0,crea$Age-1,crea$Age)
save(crea, file = "crea2.rda")
crea$log_CREA<-log(crea$Creatinine)

colnames(crea)[2] <- "PatientID"
colnames(crea)[3] <- "event.date"
colnames(crea)[18] <- "Gender"
crea<-crea[,c(2,3,18,21:24,26:28)]
save(crea, file = "crea2.rda")
########################################################################################################

#Add demographic variables from lookup tables
CPRD2<-read_dta("hf_cases_clinical.dta")  

#CODE ETHNICITY
eth<-read.table("ethnicitycprd.csv",header=TRUE,sep=",")
eth<-eth[,c(1,5)]
WHICH TABLE ARE THE ETH MEDCODES IN?
CPRD2<-merge(CPRD2,eth,by.x="medcode",by.y="medcode",all=TRUE)
CPRD2<-CPRD2[!is.na(CPRD2$Category),]

table(CPRD2$Category)
CPRD2<-CPRD2[,c(2,12)]
CPRD2<-unique(CPRD2)
head(CPRD2[duplicated(CPRD2),])
#THERE ARE NO PATIENTS WITH CONFLICTING ETHNICITY CODES

colnames(CPRD2)[1] <- "PatientID"
colnames(CPRD2)[2] <- "Ethnicity"
crea<-merge(crea,CPRD2,all.x=TRUE)

#Add LSOA
IMD<-read.csv("patient_imd2010_16_241RMnA.txt",sep="\t")
IMD<-IMD[,c(1,3)]
colnames(IMD)<-c("PatientID","IMD_Decile2010")
crea<-merge(crea,IMD,all.x=TRUE)

#CHECK ALSO FOR 23955 generic code x1
#DRAW INFORMATION ON 23955 FROM LOOKUP TABLE
CPRD2<-read_dta("hf_cases_clinical.dta")  
C2<-CPRD2[CPRD2$medcode=="23955",]
table(C2$adid) #Not linked to text

#################################################################################################################
#COHORT SELECTION
#LIMIT TO PATIENTS WITH AT LEAST 2 (POST 2008?) CREATININE TEST VALUES
#crea<-crea[as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d")))>=2008,]
table(crea$PatientID) < 2 -> rare  
rownames(as.matrix(rare)) -> ids
table(rare)
crea[!(crea$PatientID %in% ids[rare]),] -> crea.rep	
levels(unique(as.factor(crea.rep$PatientID))) # 198543 adult hf patients have 2 or more creatinine tests
length(crea.rep$PatientID) #1579775 rows remaining
##########################################################################

#SELECT PATIENTS WHICH HAVE AT LEAST 2 TESTS AND AT LEAST 2 YEARS DATA
#FIND MIN AND MAX DATES PER PATIENT
x<-CPRD2[,c(1,2)]
x1<-x %>% 
group_by(patid) %>%
summarize(start=min(eventdate),end=max(eventdate)) %>%
as.data.frame
x1$range<-as.numeric(as.character(x1$end-x1$start))

x1[(which(x1$range<2)),1] -> range_short_ids    # define exclusion range as 2 years
crea[-which(crea$PatientID %in% range_short_ids),]->crea.rep 
length(unique(crea.rep$PatientID))
save(crea.rep,file="crea.rep.rda")

save(crea,file="crea.repongoing.rda")

library("zoo")
library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("tidyverse")
library("zoo")
library("data.table")
library("survival")
library("lubridate")

load(file = "crea.repongoing_afterconditions.rda")

#CODE SIMPLE FORMULA VARIABLES
crea.rep$Ethnicity<-ifelse(is.na(crea.rep$Ethnicity),7,crea.rep$Ethnicity) #7 is used for an 'na' category so all rows continue into the clustering
crea.rep$log_CREA <- log10(crea.rep$Creatinine)
crea.rep$PP<-crea.rep$SBP-crea.rep$DBP
crea.rep$HTN<-ifelse(crea.rep$SBP>140|crea.rep$DBP>90,1,0)
crea.rep$HTN<-ifelse(is.na(crea.rep$HTN),0,crea.rep$HTN)
crea.rep$Anaemia<-ifelse(!is.na(crea.rep$Haemoglobin)&((crea.rep$Gender=="M" & crea.rep$Haemoglobin<130)|(crea.rep$Gender== "F" & crea.rep$Haemoglobin<115)),1,0)
crea.rep$Prehf<-ifelse(crea.rep$hfdate>crea.rep$event.date,1,0)
crea.rep$TimeSincehf<-difftime(strptime(crea.rep$hfdate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")

#FOR CPRD ONLY:
crea.rep$Gender<-ifelse(crea.rep$Gender==1,paste("M"),paste(crea.rep$Gender))
crea.rep$Gender<-ifelse(crea.rep$Gender=="2",paste("F"),paste(crea.rep$Gender))

#MDRD eGFR
o<-ifelse(crea.rep$Ethnicity==4,1.212,1)
o<-ifelse(is.na(crea.rep$Ethnicity),1,o)
p<-ifelse(crea.rep$Gender=="F",0.742,1)
crea.rep$MDRDeGFR<-(175*((crea.rep$Creatinine/88.42)^-1.154))*(crea.rep$Age^-0.203)*o*p

## CKD-EPI eGFR equation
## ethnicity 4=Black African/Caribbean
crea.rep$CKDEPIeGFR<- ifelse(crea.rep$Gender=="M" & crea.rep$Ethnicity==4, (141*(pmin((crea.rep$Creatinine/79.6),1)^(-0.411))*
                                                                              (pmax((crea.rep$Creatinine/79.6), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.159),  NA)
crea.rep$CKDEPIeGFR<- ifelse(is.na(crea.rep$CKDEPIeGFR) & crea.rep$Gender=="M"& crea.rep$Ethnicity!=4, (141*(pmin((crea.rep$Creatinine/79.6),1)^(-0.411))*(pmax((crea.rep$Creatinine/79.6), 1)^(-1.209))*(0.993^(crea.rep$Age))),  crea.rep$CKDEPIeGFR)
crea.rep$CKDEPIeGFR <-ifelse(is.na(crea.rep$CKDEPIeGFR) & crea.rep$Gender=="F" & crea.rep$Ethnicity==4, (141*(pmin((crea.rep$Creatinine/61.9),1)^(-0.329))*(pmax((crea.rep$Creatinine/61.9), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.018*1.159),  crea.rep$CKDEPIeGFR)
crea.rep$CKDEPIeGFR <-ifelse(is.na(crea.rep$CKDEPIeGFR), (141*(pmin((crea.rep$Creatinine/61.9),1)^(-0.329))*(pmax((crea.rep$Creatinine/61.9), 1)^(-1.209))*(0.993^(crea.rep$Age))*1.018), crea.rep$CKDEPIeGFR)

save(crea.rep, file = "crea.repongoing_witheGFR.rda")

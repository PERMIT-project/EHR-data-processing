#########################################################
#VARIABLES WITH START AND ONGOING DATES- MATCH FIRST OCCURENCE
##########################################################
#THE FOLLOWING VARIABLES ARE CREATED BASED ON LOOKUP TABLES OF CLINICAL CODES USED TO QUERY AN EHR EXTRACT TABLE.
#THREE COLUMNS ARE CREATED FOR EACH METRIC- TIME SINCE EVENT (DaysSince*), EVENT DATE (*Date), AND A BINARY MARKER (Condition_name) SHOWING EVER OCCURENCE

library(dplyr)
################################################################################
#ATRIAL FIBRILLATION
AF1<-read.csv("AF1.csv")
CPRD2$tempDate<-CPRD2$eventdate 
CPRD2$tempDate<-ifelse(!CPRD2$medcode  %in%  AF1$Medcode,NA,CPRD2$tempDate) 
CPRD3$tempDate<-CPRD3$eventdate 
CPRD3$tempDate<-ifelse(!CPRD3$medcode  %in%  AF1$Medcode,NA,CPRD3$tempDate) 

smalltab<-rbind(CPRD2[,c(1,12)],CPRD3[,c(1,18)])
smalltab<-smalltab[!is.na(smalltab$tempDate),]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(tempDate) %>%
    slice(which.min(as.numeric(tempDate))) %>%
as.data.frame
    head(first)
    
names(first)<-c("PatientID","AFDate")
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)

crea.rep$AFDate<-as.Date(crea.rep$AFDate,origin="1970/01/01")
crea.rep$DaysSinceAF<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$AFDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceAF<-ifelse(crea.rep$DaysSinceAF<0,NA,crea.rep$DaysSinceAF)
crea.rep$AF<-ifelse(is.na(crea.rep$DaysSinceAF),0,1)
summary(crea.rep$DaysSinceAF,na.rm=TRUE)

##############################################################################
#IHD
IHD1<-read.csv("IHD1.csv")
CPRD2$tempDate<-CPRD2$eventdate 
CPRD2$tempDate<-ifelse(!CPRD2$medcode  %in%  IHD1$Medcode,NA,CPRD2$tempDate) 
CPRD3$tempDate<-CPRD3$eventdate 
CPRD3$tempDate<-ifelse(!CPRD3$medcode  %in%  IHD1$Medcode,NA,CPRD3$tempDate) 

smalltab<-rbind(CPRD2[,c(1,12)],CPRD3[,c(1,18)])
smalltab<-smalltab[!is.na(smalltab$tempDate),]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(tempDate) %>%
    slice(which.min(as.numeric(tempDate))) %>%
as.data.frame
    head(first)
    
names(first)<-c("PatientID","IHDDate")
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)

crea.rep$IHDDate<-as.Date(crea.rep$IHDDate,origin="1970/01/01")
crea.rep$DaysSinceIHD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$IHDDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceIHD<-ifelse(crea.rep$DaysSinceIHD<0,NA,crea.rep$DaysSinceIHD)
crea.rep$IHD<-ifelse(is.na(crea.rep$DaysSinceIHD),0,1)
summary(crea.rep$DaysSinceIHD,na.rm=TRUE)


##################################################################################
#PVD
PVD1<-read.csv("PVD1.csv")
CPRD2$tempDate<-CPRD2$eventdate 
CPRD2$tempDate<-ifelse(!CPRD2$medcode  %in%  PVD1$Medcode,NA,CPRD2$tempDate) 
CPRD3$tempDate<-CPRD3$eventdate 
CPRD3$tempDate<-ifelse(!CPRD3$medcode  %in%  PVD1$Medcode,NA,CPRD3$tempDate) 

smalltab<-rbind(CPRD2[,c(1,12)],CPRD3[,c(1,18)])
smalltab<-smalltab[!is.na(smalltab$tempDate),]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(tempDate) %>%
    slice(which.min(as.numeric(tempDate))) %>%
as.data.frame
    head(first)
    
names(first)<-c("PatientID","PVDDate")
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)

crea.rep$PVDDate<-as.Date(crea.rep$PVDDate,origin="1970/01/01")
crea.rep$DaysSincePVD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$PVDDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSincePVD<-ifelse(crea.rep$DaysSincePVD<0,NA,crea.rep$DaysSincePVD)
crea.rep$PVD<-ifelse(is.na(crea.rep$DaysSincePVD),0,1)
summary(crea.rep$DaysSincePVD,na.rm=TRUE)
save(crea.rep, file = "crea.repongoing.rda")

######################################################################################
#NEPHRECTOMY
Nep<-read.csv("Nephrectomy1.csv")
head(CPRD2[CPRD2$medcode %in% Nep$Medcode,])# CHECK THERE ARE VALUES TO MATCH
CPRD2$NephDate<-CPRD2$eventdate 
CPRD2$NephDate<-ifelse(!CPRD2$medcode  %in%  Nep$Medcode,NA,paste(CPRD2$NephDate)) 

smalltab<-CPRD2[!is.na(CPRD2$NephDate) & CPRD2$patid %in% crea.rep$PatientID,c("patid","NephDate")]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(NephDate) %>%
    slice(1L) %>%
as.data.frame
head(first)
names(first)[1]<-"PatientID"

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceNephrectomy<-difftime(strptime(crea.rep$NephDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceNephrectomy<-as.numeric(crea.rep$DaysSinceNephrectomy, units="days")
crea.rep$DaysSinceNephrectomy<-ifelse(crea.rep$DaysSinceNephrectomy<0,NA,crea.rep$DaysSinceNephrectomy)
save(crea.rep, file = "crea.repongoing.rda")

######################################################################################
#TRANSPLANT
Nep<-read.csv("transplant1.csv")
head(CPRD2[CPRD2$medcode %in% Nep$Medcode,])# CHECK THERE ARE VALUES TO MATCH
CPRD2$TransplantDate<-CPRD2$eventdate 
CPRD2$TransplantDate<-ifelse(!CPRD2$medcode  %in%  Nep$Medcode,NA,paste(CPRD2$TransplantDate)) 

smalltab<-CPRD2[!is.na(CPRD2$TransplantDate) & CPRD2$patid %in% crea.rep$PatientID,c("patid","TransplantDate")]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(TransplantDate) %>%
    slice(1L) %>%
as.data.frame
head(first)
names(first)[1]<-"PatientID"

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceTransplant<-difftime(strptime(crea.rep$TransplantDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceTransplant<-as.numeric(crea.rep$DaysSinceTransplant, units="days")
crea.rep$DaysSinceTransplant<-ifelse(crea.rep$DaysSinceTransplant<0,NA,crea.rep$DaysSinceTransplant)
save(crea.rep, file = "crea.repongoing.rda")
######################################################################################
#RRT
Dialysis<-read.csv("Dialysis1.csv")
Transplant<-read.csv("transplant1.csv")

head(CPRD2[CPRD2$medcode %in% Transplant$Medcode| CPRD2$medcode %in% Dialysis$Medcode,])# CHECK THERE ARE VALUES TO MATCH
CPRD2$RRTDate<-CPRD2$eventdate 
CPRD2$RRTDate<-ifelse(!CPRD2$medcode  %in%  Transplant$Medcode & !CPRD2$medcode  %in%  Dialysis$Medcode,NA,paste(CPRD2$RRTDate)) 

smalltab<-CPRD2[!is.na(CPRD2$RRTDate) & CPRD2$patid %in% crea.rep$PatientID,c("patid","RRTDate")]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(RRTDate) %>%
    slice(1L) %>%
as.data.frame
head(first)
names(first)[1]<-"PatientID"

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceRRT<-difftime(strptime(crea.rep$RRTDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceRRT<-as.numeric(crea.rep$DaysSinceRRT, units="days")
crea.rep$DaysSinceRRT<-ifelse(crea.rep$DaysSinceRRT<0,NA,crea.rep$DaysSinceRRT)
crea.rep$RenalTransplant<-ifelse(crea.rep$RenalTransplant==1,1,0)
crea.rep$RenalTransplant<-ifelse(is.na(crea.rep$RenalTransplant),0,1)
save(crea.rep, file = "crea.repongoing.rda")
######################################################################################

#DIABETES
Diabetes<-read.csv("Diabetes1.csv")
head(CPRD2[CPRD2$medcode %in% Diabetes$Medcode,])# CHECK THERE ARE VALUES TO MATCH
CPRD2$DiabDate<-CPRD2$eventdate 
CPRD2$DiabDate<-ifelse(!CPRD2$medcode  %in%  Diabetes$Medcode ,NA,paste(CPRD2$DiabDate)) 

smalltab<-CPRD2[!is.na(CPRD2$DiabDate) & CPRD2$patid %in% crea.rep$PatientID,c("patid","DiabDate")]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(DiabDate) %>%
    slice(1L) %>%
as.data.frame
head(first)
names(first)[1]<-"PatientID"

crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceDiabetic<-difftime(strptime(crea.rep$DiabDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceDiabetic<-as.numeric(crea.rep$DaysSinceDiabetic, units="days")
crea.rep$DaysSinceDiabetic<-ifelse(crea.rep$DaysSinceDiabetic<0,NA,crea.rep$DaysSinceDiabetic)
save(crea.rep, file = "crea.repongoing.rda")

########################################################################################

#RENAL MALIGNANCY
sir.data$RMALDate<-sir.data$EntryDate 
sir.data$RMALDate<-ifelse(!sir.data$ReadCode  %in%  RM1.csv$ReadCode,NA,sir.data$RMALDate) 
smalltab<-sir.data[!is.na(sir.data$RMALDate),c("PatientID","RMALDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(RMALDate) %>%
    slice(which.min(as.numeric(RMALDate))) %>%
as.data.frame
    head(first)
    
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
crea.rep$event.date<-as.Date(as.character(crea.rep$EntryDate),format="%Y%m%d")
crea.rep$RMALDate<-as.Date(as.character(crea.rep$RMALDate),format="%Y%m%d")
crea.rep$DaysSinceRenMal<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$RMALDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceRenMal<-ifelse(crea.rep$DaysSinceRenMal<0,NA,crea.rep$DaysSinceRenMal)
crea.rep$RenalMalignancy<-ifelse(is.na(crea.rep$DaysSinceRenMal),0,1)
summary(crea.rep$DaysSinceRenMal,na.rm=TRUE)

save(crea.rep, file = "crea.repongoing.rda")

########################################################### 
#MATERNITY 
Maternity1.csv<-read.csv("Maternity1.csv")
CPRD2$Maternity<-ifelse(CPRD2$medcode  %in%  Maternity1.csv$Medcode,1,0) 
CPRD3$Maternity<-ifelse(CPRD3$medcode  %in%  Maternity1.csv$Medcode,1,0) 
tab1<-CPRD2[CPRD2$Maternity==1 & CPRD2$patid %in% crea.rep$PatientID,c("patid","Maternity","eventdate")]
tab2<-CPRD3[CPRD3$Maternity==1 & CPRD3$patid %in% crea.rep$PatientID,c("patid","Maternity","eventdate")]
smalltab<-rbind(tab1,tab2)

names(smalltab)[3]<-"MatDate1"
smalltab<-smalltab[smalltab$MatDate1>="2007-01-01",]
first<-smalltab %>%
   group_by(patid) %>%  
      slice(which.min(as.numeric(MatDate1))) %>%
as.data.frame
head(first)

#NB THIS IS THE FIRST RECORDED PREGANANCY DATE FOR THE PATIENT'S FIRST POST-2007 BABY, NOT NECESSARILY THE FIRST CHILD.

names(smalltab)[3]<-"MatDate2"
smalltab<-merge(smalltab[,c(1,3)],first[,c(1,3)],all.x=TRUE)

smalltab$MatDate2<-as.Date(as.character(smalltab$MatDate2),"%Y-%m-%d")
smalltab$MatDate1<-as.Date(as.character(smalltab$MatDate1),"%Y-%m-%d")
smalltab$d<-as.numeric(smalltab$MatDate2-smalltab$MatDate1)
second<-smalltab[smalltab$d>=310,]

s<-second %>%
   group_by(patid) %>%  
      slice(which.min(as.numeric(MatDate2))) %>%
      as.data.frame
        head(s)
        
smalltab2<-rbind(tab1,tab2)
names(smalltab2)[3]<-"MatDate3"   
smalltab2$MatDate3<-as.Date(as.character(smalltab2$MatDate3),"%Y-%m-%d")
smalltab2<-smalltab2[smalltab2$MatDate3>="2007-01-01",]
smalltab2<-merge(smalltab2[,c(1,3)],s,all.x=TRUE)     
smalltab2<-unique(smalltab2)
smalltab2$MatDate2<-as.Date(as.character(smalltab$MatDate2),"%Y-%m-%d")
smalltab2$MatDate3<-as.Date(as.character(smalltab2$MatDate3),"%Y-%m-%d")
smalltab2$d2<-as.numeric(smalltab2$MatDate3-smalltab2$MatDate2)
third<-smalltab2[smalltab2$d2>=310,]

t<-third %>%
   group_by(patid) %>%  
      slice(which.min(as.numeric(MatDate3))) %>%
      as.data.frame
        head(t)
        
        length(t$patid)
#ONLY 27 HF PATIENTS HAD 3 OR MORE CHILDREN POST 2007
        
smalltab3<-merge(unique(smalltab2[,c(1,3,4)]),t[,c(1,2)],all.x=TRUE)     
smalltab3<-unique(smalltab3)

smalltab4<-rbind(tab1,tab2)
names(smalltab4)[3]<-"MatDate4"   
smalltab4<-smalltab4[smalltab4$MatDate4>="2007-01-01",]
smalltab4<-merge(smalltab4[,c(1,3)],t,all.x=TRUE)     
smalltab4<-unique(smalltab4)
smalltab4$MatDate4<-as.Date(as.character(smalltab4$MatDate4),"%Y-%m-%d")
smalltab4$d3<-as.numeric(smalltab4$MatDate4-smalltab4$MatDate3)
fourth<-smalltab4[smalltab4$d3>=310,]
length(unique(fourth$patid))

#7 PATIENTS HAD 4 OR MORE CHILDREN

f<-fourth %>%
   group_by(patid) %>%  
      slice(which.min(as.numeric(MatDate4))) %>%
      as.data.frame
        head(f)

smalltab5<-merge(unique(smalltab4[,c(1,3:5)]),f[,c(1,2)],all.x=TRUE) 
crea.rep<-merge(crea.rep,smalltab5,all.x=TRUE)

crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate1<310,1,0)
crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate2<310,1,crea.rep$EstPregnant)
crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate3<310,1,crea.rep$EstPregnant)
crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate4<310,1,crea.rep$EstPregnant)
save(crea.rep, file = "crea.rep.rda")

################################################################################
#CHRONIC LIVER DISEASE
CLD1<-read.csv("Liver1.csv")
CPRD2$tempDate<-CPRD2$eventdate 
CPRD2$tempDate<-ifelse(!CPRD2$medcode  %in%  CLD1$Medcode,NA,CPRD2$tempDate) 
CPRD3$tempDate<-CPRD3$eventdate 
CPRD3$tempDate<-ifelse(!CPRD3$medcode  %in%  CLD1$Medcode,NA,CPRD3$tempDate) 

smalltab<-rbind(CPRD2[,c(1,12)],CPRD3[,c(1,18)])
smalltab<-smalltab[!is.na(smalltab$tempDate),]
first<-smalltab %>%
    group_by(patid) %>%
    arrange(tempDate) %>%
    slice(which.min(as.numeric(tempDate))) %>%
as.data.frame
    head(first)
    
names(first)<-c("PatientID","CLD1Date")
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)

crea.rep$CLDDate<-as.Date(crea.rep$CLDDate,origin="1970/01/01")
crea.rep$DaysSinceCLD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$CLDDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceCLD<-ifelse(crea.rep$DaysSinceCLD<0,NA,crea.rep$DaysSinceCLD)
crea.rep$CLD<-ifelse(is.na(crea.rep$DaysSinceCLD),0,1)
summary(crea.rep$DaysSinceCLD,na.rm=TRUE)


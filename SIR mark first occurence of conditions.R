#########################################################
#VARIABLES WITH START AND ONGOING DATES- MATCH FIRST OCCURENCE
##########################################################
#THE FOLLOWING VARIABLES ARE CREATED BASED ON LOOKUP TABLES OF CLINICAL CODES USED TO QUERY AN EHR EXTRACT TABLE.
#THREE COLUMNS ARE CREATED FOR EACH METRIC- TIME SINCE EVENT (DaysSince*), EVENT DATE (*Date), AND A BINARY MARKER (Condition_name) SHOWING EVER OCCURENCE

load("sirdatahfonly.rda") #Load the full table
load("crea.rep.rda")

#NEPHRECTOMY
sir.data$NephDate<-sir.data$EntryDate 
sir.data$NephDate<-ifelse(!sir.data$ReadCode  %in%  Nephrectomy1.csv$ReadCode,NA,sir.data$NephDate) 

smalltab<-sir.data[!is.na(sir.data$NephDate) & sir.data$PatientID %in% crea.rep$PatientID,c("PatientID","NephDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(NephDate) %>%
    slice(1L) %>%
as.data.frame
head(first)
first$NephDate<-as.Date(as.character(first$NephDate),format="%Y%m%d")
 
crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$event.date<-as.Date(as.character(crea.rep$EntryDate),format="%Y%m%d")
crea.rep$NephDate<-as.Date(as.character(crea.rep$NephDate),format="%Y-%m-%d")
crea.rep$DaysSinceNephrectomy<-difftime(strptime(crea.rep$NephDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceNephrectomy<-as.numeric(crea.rep$DaysSinceNephrectomy, units="days")
crea.rep$DaysSinceNephrectomy<-ifelse(crea.rep$DaysSinceNephrectomy<0,NA,crea.rep$DaysSinceNephrectomy)
crea.rep$Nephrectomy<-ifelse(is.na(crea.rep$DaysSinceNephrectomy),0,1)



#RRT
sir.data$RRTDate<-sir.data$EntryDate 
sir.data$RRTDate<-ifelse(!sir.data$ReadCode  %in%  Dialysis1.csv$ReadCode,NA,sir.data$RRTDate) 
smalltab<-sir.data[!is.na(sir.data$RRTDate),c("PatientID","RRTDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(RRTDate) %>%
    slice(which.min(as.numeric(RRTDate))) %>%
as.data.frame
    head(first)
   
first$RRTDate<-as.Date(as.character(first$RRTDate),format="%Y%m%d")
crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceRRT<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$RRTDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceRRT<-ifelse(crea.rep$DaysSinceRRT<0,NA,crea.rep$DaysSinceRRT)
crea.rep$RRT<-ifelse(is.na(crea.rep$DaysSinceRRT),0,1)


#DIABETES
sir.data$DiabDate<-sir.data$EntryDate 
sir.data$DiabDate<-ifelse(!sir.data$ReadCode  %in%  Diabetes1.csv$ReadCode,NA,sir.data$DiabDate) 
smalltab<-sir.data[!is.na(sir.data$DiabDate),c("PatientID","DiabDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(DiabDate) %>%
    slice(which.min(as.numeric(DiabDate))) %>%
as.data.frame
    head(first)
   
first$DiabDate<-as.Date(as.character(first$DiabDate),format="%Y%m%d")
crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$DaysSinceDiabetic<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$DiabDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceDiabetic<-ifelse(crea.rep$DaysSinceDiabetic<0,NA,crea.rep$DaysSinceDiabetic)
crea.rep$Diabetes<-ifelse(is.na(crea.rep$DaysSinceDiabetic),0,1)


#ATRIAL FIBRILLATION
sir.data$AFDate<-sir.data$EntryDate 
sir.data$AFDate<-ifelse(!sir.data$ReadCode  %in%  AF1.csv$ReadCode,NA,sir.data$AFDate) 
smalltab<-sir.data[!is.na(sir.data$AFDate),c("PatientID","AFDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(AFDate) %>%
    slice(which.min(as.numeric(AFDate))) %>%
as.data.frame
    head(first)
    
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
crea.rep$event.date<-as.Date(as.character(crea.rep$EntryDate),format="%Y%m%d")
crea.rep$AFDate<-as.Date(as.character(crea.rep$AFDate),format="%Y%m%d")
crea.rep$DaysSinceAF<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$AFDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceAF<-ifelse(crea.rep$DaysSinceAF<0,NA,crea.rep$DaysSinceAF)
crea.rep$AF<-ifelse(is.na(crea.rep$DaysSinceAF),0,1)
summary(crea.rep$DaysSinceAF,na.rm=TRUE)


#IHD
sir.data$IHDDate<-sir.data$EntryDate 
sir.data$IHDDate<-ifelse(!(sir.data$ReadCode  %in%  IHD1.csv$ReadCode)&!(sir.data$ReadCode  %in%  IHD1.csv$MedCode),NA,sir.data$IHDDate) 
smalltab<-sir.data[!is.na(sir.data$IHDDate),c("PatientID","IHDDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(IHDDate) %>%
    slice(which.min(as.numeric(IHDDate))) %>%
as.data.frame
    head(first)
   
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
crea.rep$IHDDate<-as.Date(as.character(crea.rep$IHDDate),format="%Y%m%d")
crea.rep$DaysSinceIHD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$IHDDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceIHD<-ifelse(crea.rep$DaysSinceIHD<0,NA,crea.rep$DaysSinceIHD)
crea.rep$IHD<-ifelse(is.na(crea.rep$DaysSinceIHD),0,1)
summary(crea.rep$DaysSinceIHD,na.rm=TRUE)


#PERIPHERAL VASCULAR DISEASE
sir.data$PVDDate<-sir.data$EntryDate 
sir.data$PVDDate<-ifelse(!sir.data$ReadCode  %in%  PVD1.csv$ReadCode,NA,sir.data$PVDDate) 
smalltab<-sir.data[!is.na(sir.data$PVDDate),c("PatientID","PVDDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(PVDDate) %>%
    slice(which.min(as.numeric(PVDDate))) %>%
as.data.frame
    head(first)
    
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
crea.rep$PVDDate<-as.Date(as.character(crea.rep$PVDDate),format="%Y%m%d")
crea.rep$DaysSincePVD<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$PVDDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSincePVD<-ifelse(crea.rep$DaysSincePVD<0,NA,crea.rep$DaysSincePVD)
crea.rep$PVD<-ifelse(is.na(crea.rep$DaysSincePVD),0,1)
summary(crea.rep$DaysSincePVD,na.rm=TRUE)


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


#RENAL TRANSPLANT
sir.data$RTDate<-sir.data$EntryDate 
sir.data$RTDate<-ifelse(!sir.data$ReadCode  %in%  transplant1.csv$ReadCode,NA,sir.data$RTDate) 
smalltab<-sir.data[!is.na(sir.data$RTDate),c("PatientID","RTDate")]
first<-smalltab %>%
   group_by(PatientID) %>%  
   arrange(RTDate) %>% 
   slice(which.min(as.numeric(RTDate))) %>%
as.data.frame    
head(first)
  
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
crea.rep$event.date<-as.Date(as.character(crea.rep$EntryDate),format="%Y%m%d")
crea.rep$RTDate<-as.Date(as.character(crea.rep$RTDate),format="%Y%m%d")
crea.rep$DaysSinceRT<-difftime(strptime(crea.rep$event.date,format="%Y-%m-%d"),strptime(crea.rep$RTDate,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceRT<-ifelse(crea.rep$DaysSinceRT<0,NA,crea.rep$DaysSinceRT)
crea.rep$RenalTransplant<-ifelse(is.na(crea.rep$DaysSinceRT),0,1)
summary(crea.rep$DaysSinceRT,na.rm=TRUE)

#DEATH
sir.data$Dead<-ifelse(sir.data$ReadCode  %in%  Death1.csv$ReadCode,1,0) 
smalltab<-sir.data[sir.data$Dead==1,c("PatientID","Dead")]
first<-smalltab %>%
   group_by(PatientID) %>%  
      slice(which.max(as.numeric(Dead))) %>%
as.data.frame    
head(first)
  
crea.rep<-merge(crea.rep,as.data.frame(first),all.x=TRUE)
summary(crea.rep$Dead,na.rm=TRUE)
#No death dates were associated with the extracted data for this analysis
#save(crea.rep, file = "crea.repongoing.rda")
 

#CLD
sir.data$CLDDate<-sir.data$EntryDate 
sir.data$CLDDate<-ifelse(!sir.data$ReadCode  %in%  Liver1.csv$ReadCode,NA,sir.data$CLDDate) 

smalltab<-sir.data[!is.na(sir.data$CLDDate) & sir.data$PatientID %in% crea.rep$PatientID,c("PatientID","CLDDate")]
first<-smalltab %>%
    group_by(PatientID) %>%
    arrange(CLDDate) %>%
    slice(1L) %>%
as.data.frame
first$CLDDate<-as.Date(as.character(first$CLDDate),format="%Y%m%d")
head(first)
 
crea.rep<-merge(crea.rep,first,all.x=TRUE)
crea.rep$event.date<-as.Date(as.character(crea.rep$EntryDate),format="%Y%m%d")
crea.rep$CLDDate<-as.Date(as.character(crea.rep$CLDDate),format="%Y-%m-%d")
crea.rep$DaysSinceCLD<-difftime(strptime(crea.rep$CLDDate,format="%Y-%m-%d"),strptime(crea.rep$event.date,format="%Y-%m-%d"),unit="days")
crea.rep$DaysSinceCLD<-as.numeric(crea.rep$DaysSinceCLD, units="days")
crea.rep$DaysSinceCLD<-ifelse(crea.rep$DaysSinceCLD<0,NA,crea.rep$DaysSinceCLD)
crea.rep$CLD<-ifelse(crea.rep$DaysSinceCLD>=0,1,0)


#MATERNITY 
allfiles = list.files(pattern="*1.csv")
for (i in 1:length(allfiles)) assign(allfiles[i], read.csv(allfiles[i]))

sir.data$Maternity<-ifelse(sir.data$ReadCode  %in%  Maternity1.csv$ReadCode,1,0) 
smalltab<-sir.data[sir.data$Maternity==1 & sir.data$PatientID %in% crea.rep$PatientID,c("PatientID","Maternity","EntryDate")]
names(smalltab)[3]<-"MatDate1"
smalltab<-smalltab[smalltab$MatDate1>="2007-01-01",]
first<-smalltab %>%
   group_by(PatientID) %>%  
      slice(which.min(as.numeric(MatDate1))) %>%
as.data.frame
head(first)

#MARKS  FIRST RECORDED PREGANANCY DATE FOR THE PATIENT'S FIRST POST-2007 BABY- NOT NECESSARILY THE FIRST CHILD.

names(smalltab)[3]<-"MatDate2"
smalltab<-merge(smalltab[,c(1,3)],first[,c(1,3)],all.x=TRUE)

smalltab$MatDate2<-as.Date(as.character(smalltab$MatDate2),"%Y%m%d")
smalltab$MatDate1<-as.Date(as.character(smalltab$MatDate1),"%Y%m%d")
smalltab$d<-as.numeric(smalltab$MatDate2-smalltab$MatDate1)
second<-smalltab[smalltab$d>=243,]

s<-second %>%
   group_by(PatientID) %>%  
      slice(which.min(as.numeric(MatDate2))) %>%
      as.data.frame
        head(s)
        
smalltab2<-sir.data[sir.data$Maternity==1 & sir.data$PatientID %in% crea.rep$PatientID,c("PatientID","Maternity","EntryDate")]
names(smalltab2)[3]<-"MatDate3"   
smalltab2$MatDate3<-as.Date(as.character(smalltab2$MatDate3),"%Y%m%d")
smalltab2<-smalltab2[smalltab2$MatDate3>="2007-01-01",]
smalltab2<-merge(smalltab2[,c(1,3)],s,all.x=TRUE)     
smalltab2<-unique(smalltab2)
smalltab2$d2<-as.numeric(smalltab2$MatDate3-smalltab2$MatDate2)
third<-smalltab2[smalltab2$d2>=243,]
third<-unique(third)
   
crea.rep<-merge(crea.rep,unique(smalltab2[,c(1,3,4)]),all.x=TRUE)
crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate1<243,1,NA)
crea.rep$EstPregnant<-ifelse(crea.rep$event.date-crea.rep$MatDate2<243,1,crea.rep$EstPregnant)

#IF APPLYING A DATE RANGE EXCLUDION, DROP OUT OF RANGE ENTRIES:

crea.rep$BNP_DF<-ifelse(crea.rep$BNPDateFlag==1,NA,crea.rep$BNP)
crea.rep$NTPROBNP_DF<-ifelse(crea.rep$NTPROBNPDateFlag==1,NA,crea.rep$NTPROBNP)
crea.rep$SBP_DF<-ifelse(crea.rep$SBPDateFlag==1,NA,crea.rep$SBP)
crea.rep$DBP_DF<-ifelse(crea.rep$DBPDateFlag==1,NA,crea.rep$DBP)
crea.rep$HeartRate_DF<-ifelse(crea.rep$HeartRateDateFlag==1,NA,crea.rep$HeartRate)
crea.rep$BMI_DF<-ifelse(crea.rep$BMIDateFlag==1,NA,crea.rep$BMI)
crea.rep$SerumAlbumin_DF<-ifelse(crea.rep$SerumAlbuminDateFlag==1,NA,crea.rep$SerumAlbumin)
crea.rep$UrineAlbumin_DF<-ifelse(crea.rep$UrineAlbuminDateFlag==1,NA,crea.rep$UrineAlbumin)
crea.rep$UACratio_DF<-ifelse(crea.rep$UACDateFlag==1,NA,crea.rep$UACratio)
crea.rep$Haemoglobin_DF<-ifelse(crea.rep$HaemDateFlag==1,NA,crea.rep$Haemoglobin)
crea.rep$MCV_DF<-ifelse(crea.rep$MCVDateFlag==1,NA,crea.rep$MCV)
crea.rep$SerPotassium_DF<-ifelse(crea.rep$SerPotDateFlag==1,NA,crea.rep$SerPotassium)
crea.rep$SerumSodium_DF<-ifelse(crea.rep$SerSodDateFlag==1,NA,crea.rep$SerumSodium)
crea.rep$BUN_DF<-ifelse(crea.rep$BUNDateFlag==1,NA,crea.rep$BUN)
crea.rep$UricAcid_DF<-ifelse(crea.rep$UricAcidDateFlag==1,NA,crea.rep$UricAcid)

save(crea.rep, file = "crea.repongoing.rda")

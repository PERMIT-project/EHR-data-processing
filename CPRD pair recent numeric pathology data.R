#################################################################################################################
#ADD CONDITIONAL VARIABLES
library(dplyr)
#ADD BMI
#Call the data fields that go with the clinical table from the 'additional' table using enttype and adid as a key.

#There are some in test, also some in clinical, 
BMI<-read.csv("BMI1.csv")
C<-(CPRD3[CPRD3$medcode %in% BMI$Medcode,c(1,2,11)])
C<-as.data.frame(C)
colnames(C)[1] <- "PatientID"
colnames(C)[3] <- "BMI"

C2<-(CPRD2[CPRD2$medcode %in% BMI$Medcode | CPRD2$enttype==13,])
C2<-as.data.frame(C2)
C2<-merge(C2,CPRD5,all.x=TRUE,all.y=FALSE) 
colnames(C2)[1] <- "PatientID"
C2<-C2[!is.na(C2$data3),]
C2<-C2[,c(1,4,14)]
colnames(C2)[3] <- "BMI"
C<-rbind(C,C2)

#CLOSEST DAILY BMI MEAN #1YR DATE FLAG
BMItab<-C[C$BMI>=10 & C$BMI<=70,]
firstA <-BMItab %>% 
group_by(PatientID,eventdate) %>%
summarize(BMI=mean(BMI)) %>%
as.data.frame

indx1 <- neardate(crea$PatientID, firstA$PatientID, crea$event.date, firstA$eventdate, best="prior")
crea$BMI<-firstA[indx1, "BMI"]
crea$BMIDateFlag<- ifelse(as.numeric(abs(crea$event.date - firstA$eventdate[indx1])) >365, 1, 0)
table(crea$BMIDateFlag) 
a<-crea[!is.na(crea$BMI),]
length(a$PatientID) 
crea$BMI<-unlist(crea$BMI)
crea$BMIDateFlag<-ifelse(is.na(crea$BMIDateFlag),0,crea$BMIDateFlag)
save(crea,file="creaongoing.rda")
rm(a)
#IF BMI IS MISSING WE CAN CALCULATE IT.

Height<-read.csv("Height1.csv")
H<-(CPRD2[CPRD2$medcode %in% Height$Medcode,])
H<-as.data.frame(H)
H<-merge(H,CPRD5,all.x=TRUE,all.y=FALSE) 
H<-H[,c(1,4,12)]
colnames(H)[3] <- "Height"
colnames(H)[1] <- "PatientID"

#Near data match each creatinine record to the last (tallest) recorded height and the most recent weight
firstH<-H %>% 
group_by(PatientID) %>%
summarize(Height=max(Height)) %>%
as.data.frame


crea<-merge(crea,firstH,all.x=TRUE)
crea$Height<-unlist(crea$Height)
save(crea,file="creaongoing.rda")
rm(firstH)

Weight<-read.csv("Weight1.csv")
W<-(CPRD2[CPRD2$medcode %in% Weight$Medcode,])
W<-as.data.frame(W)
CPRD5<-read_dta("hf_cases_additional.dta") 
W<-merge(W,CPRD5,all.x=TRUE,all.y=FALSE) 
#Call the data fields that go with the clinical table from the 'additional' table using enttype and adid.
W<-W[,c(1,4,12)]
colnames(W)<- c("PatientID","event.date","Weight")

indx1 <- neardate(crea$PatientID, W$PatientID, crea$event.date, W$event.date, best="prior")
crea$Weight<-W[indx1, "Weight"]
crea$Weightdate<-W[indx1,"entry.date"]
crea$Weight<-unlist(crea$Weight)
crea$Weightdate<-unlist(crea$Weightdate)

crea$BMIDateFlag<-ifelse(is.na(crea$BMIDateFlag)&!is.na(crea$Height)&!is.na(crea$Weight)&(as.numeric(abs(crea$event.date - crea$Weightdate))>365),1,crea$BMIDateFlag)
crea$BMI<-ifelse(is.na(crea$BMI)&!is.na(crea$Height)&!is.na(crea$Weight),(crea$Weight/(crea$Height^2)),crea$BMI)
crea$BMI<-ifelse(crea$BMI>=10&crea$BMI<=70,crea$BMI,NA)
#no additional BMIs were added in this analysis

###########################################################################
#SERUM SODIUM
SerumSodium1<-read.csv("SerumSodium1.csv")
C<-(CPRD3[CPRD3$medcode %in% SerumSodium1$Medcode,c(1,2,11,12)])
table(C$data3)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR NONE)

C<-as.data.frame(C)
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=80&C$Value<=200,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(SerumSodium=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$SerumSodium<-first[indx1, "SerumSodium"]
crea.rep$SerSodDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$SerSodDateFlag) #652
crea.rep$SerumSodium<-unlist(crea.rep$SerumSodium)

###########################################################################
#SERUM POTASSIUM
SerumPotassium<-read.csv("SerumPotassium1.csv")
C<-(CPRD3[CPRD3$medcode %in% SerumPotassium$Medcode,c(1,2,11,12)])
table(C$data3)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR NONE)

C<-as.data.frame(C)
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=2 & C$Value<=10,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(SerumPotassium=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$SerumPotassium<-first[indx1, "SerumPotassium"]
crea.rep$SerPotDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$SerPotDateFlag) #652
crea.rep$SerumPotassium<-unlist(crea.rep$SerumPotassium)

###############################################################################
#URIC ACID

UA<-read.csv("UricAcid1.csv")
C<-(CPRD3[CPRD3$medcode %in% UA$Medcode,c(1,2,11,12)])
table(C$data3)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR umol)
C$data2<-ifelse(C$data3==142,C$data2/1000,C$data2)

C<-as.data.frame(C)
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=0.1 & C$Value<=1,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(UricAcid=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$UricAcid<-first[indx1, "UricAcid"]
crea.rep$UricAcidDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$UricAcidDateFlag) #652
crea.rep$UricAcid<-unlist(crea.rep$UricAcid)

save(crea,file="creaongoing.rda")


###############################################################################
#UACR
UACR<-read.csv("UACR1.csv")
C<-(CPRD3[CPRD3$medcode %in% UA$Medcode,c(1,2,11,12)])
table(C$data3)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR umol)
C$data2<-ifelse(C$data3==142,C$data2/1000,C$data2)
C$data2<-ifelse(C$data3==99,C$data2*1000,C$data2)

C<-as.data.frame(C[C$data2<=3000,])
colnames(C)<- c("PatientID","event.date","Value")

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(UACratio=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$UACratio<-first[indx1, "UACratio"]
crea.rep$UACDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$UACDateFlag) #652
crea.rep$UACratio<-unlist(crea.rep$UACratio)

save(crea,file="creaongoing.rda")

###############################################################################
#BUN
BUN<-read.csv("BUN1.csv")
C<-(CPRD3[CPRD3$medcode %in% BUN$Medcode &CPRD3$enttype==204,c(1,2,11,12)])
table(C$data3)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR NA)

C<-as.data.frame(C[C$data2>=1&C$data2<=50,])
colnames(C)<- c("PatientID","event.date","Value")

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(BUN=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$BUN<-first[indx1, "BUN"]
crea.rep$BUNDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$BUNDateFlag) #652
crea.rep$BUN<-unlist(crea.rep$BUN)

save(crea,file="creaongoing.rda")


###############################################################################
#DBP
DBP<-read.csv("DBP1.csv")
C<-(CPRD2[CPRD2$medcode  %in% DBP$Medcode,])
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
C2<-CPRD3[CPRD3$medcode %in% DBP$Medcode,]

CA<-C[C$enttype==1,c(1,4,12)]#data1
CB<-C[C$enttype==288,c(1,4,13)]#data2- THERE ARE NONE OF THESE
colnames(CA)<- c("PatientID","event.date","Value")

CB<-C2[C2$enttype==288,c(1,2,11)]#data2
CC<-C2[C2$enttype==1,c(1,2,10)]#data1- NONE OF THESE
colnames(CB)<- c("PatientID","event.date","Value")
C<-rbind(CA,CB)

C<-as.data.frame(C[C$Value>=20&C$Value<=200,])
columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(DBP=min(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$DBP<-first[indx1, "DBP"]
crea.rep$DBPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$DBPDateFlag) #652
crea.rep$DBP<-unlist(crea.rep$DBP)

save(crea,file="creaongoing.rda")


###############################################################################
#SBP
SBP<-read.csv("SBP1.csv")
C<-(CPRD2[CPRD2$medcode  %in% SBP$Medcode,])
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
C2<-CPRD3[CPRD3$medcode %in% SBP$Medcode,]

CA<-C[C$enttype==1,c(1,4,13)]#data2
colnames(CA)<- c("PatientID","event.date","Value")
CB<-C2[,c(1,2,11)]#data2
colnames(CB)<- c("PatientID","event.date","Value")
C<-rbind(CA,CB)

C<-as.data.frame(C[C$Value>=30&C$Value<=300,])
columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(SBP=min(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$SBP<-first[indx1, "SBP"]
crea.rep$SBPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$SBPDateFlag) #652
crea.rep$SBP<-unlist(crea.rep$SBP)

save(crea.rep,file="crea.rep.rda")
###############################################################################
#MEAN DAILY MEAN CORPUSCULAR VOLUME #120 DAYS
MCV<-read.csv("MCV1.csv")
C<-(CPRD3[CPRD3$medcode  %in% MCV$Medcode,])
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
table(C$data3)

C<-C[,c(1,4,10)]#data2
colnames(C)<- c("PatientID","Value","event.date")

C<-as.data.frame(C[C$Value>=50&C$Value<=150,])
columns=names(C[c(1,3)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(MCV=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$MCV<-first[indx1, "MCV"]
crea.rep$MCVDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >120, 1, 0)
table(crea.rep$MCVDateFlag) #652
crea.rep$MCV<-unlist(crea.rep$MCV)

save(crea.rep,file="crea.rep.rda")

###########################################################################
#SERUM ALBUMIN
SA<-read.csv("SerumAlbumin1.csv")
C<-(CPRD3[CPRD3$medcode %in% SA$Medcode & !CPRD3$enttype=="291",])
table(C$data3)
C$data2<-ifelse(C$data3==83,C$data2/1000, C$data2)
#CHECK FOR UNIQUE UNITS USED (mmol/L OR NONE)

C<-as.data.frame(C[,c(1,2,11)])
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=10 & C$Value<=60,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(SerumAlbumin=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$SerumAlbumin<-first[indx1, "SerumAlbumin"]
crea.rep$SADateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$SADateFlag) #652
crea.rep$SerumAlbumin<-unlist(crea.rep$SerumAlbumin)

###########################################################################
#URINE ALBUMIN
UA<-read.csv("UAlbumin1.csv")
C<-(CPRD3[CPRD3$medcode %in% UA$Medcode,])
C2<-(CPRD2[CPRD2$medcode %in% UA$Medcode,])
C2<-merge(C2,CPRD5,all.x=TRUE,all.y=FALSE) 
summary(C2)#NO numeric data

#CHECK UNITS!
unique(C$data3)
C$data2<-ifelse(C$data3==57,C$data2*1000,C$data2)
C<-C[,c(1,2,11)]
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=0 & C$Value<=1000,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(UrineAlbumin=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$UrineAlbumin<-first[indx1, "UrineAlbumin"]
crea.rep$UADateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$UADateFlag) #652
crea.rep$UrineAlbumin<-unlist(crea.rep$UrineAlbumin)

################################################################################

#HEART RATE
#data1 enttype 131
HR<-read.csv("HeartRate1.csv")
C<-(CPRD2[CPRD2$medcode %in% HR$Medcode & CPRD2$enttype=="131",])
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 

C<-as.data.frame(C[,c(1,4,13)])
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=20 & C$Value<=200,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(HeartRate=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$HeartRate<-first[indx1, "HeartRate"]
crea.rep$HeartRateDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$HeartRateDateFlag) #652
crea.rep$HeartRate<-unlist(crea.rep$HeartRate)

#####################################################################################
#BNP
BNP<-read.csv("BNP1.csv")
C<-CPRD3[CPRD3$medcode %in% BNP$Medcode,]
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
unique(C$data3)
#MANY UNITS
C$data2<-ifelse(C$data3==120,C$data2/1000,C$data2)
C<-C[C$data2>=1&C$data2<=1000,]

C<-as.data.frame(C[,c(1,10,4)])
colnames(C)<- c("PatientID","event.date","Value")
C<-na.omit(C)
columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(BNP=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$BNP<-first[indx1, "BNP"]
crea.rep$BNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$BNPDateFlag) #652
crea.rep$BNP<-unlist(crea.rep$BNP)


######################################################################################
#NTPROBNP 
NTBNP<-read.csv("NTPROBNP1.csv")
C<-CPRD3[CPRD3$medcode %in% NTBNP$Medcode,]
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
unique(C$data3)
#MANY UNITS
C$data2<-ifelse(C$data3==120,C$data2/1000,C$data2)
C$data2<-ifelse(C$data3==96,C$data2*1000000000*0.289,C$data2)
C<-C[C$data2>=1&C$data2<=6000,]

C<-as.data.frame(C[,c(1,10,4)])
colnames(C)<- c("PatientID","event.date","Value")
C<-na.omit(C)
columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(NTPROBNP=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$NTPROBNP<-first[indx1, "NTPROBNP"]
crea.rep$NTPROBNPDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >365, 1, 0)
table(crea.rep$NTPROBNPDateFlag) #652
crea.rep$NTPROBNP<-unlist(crea.rep$NTPROBNP)
#CHECK UNITS!

#####################################################################################
#Haemoglobin
HB<-read.csv("Haemoglobin1.csv")
C<-(CPRD3[CPRD3$medcode %in% HB$Medcode & CPRD3$enttype=="173",])
C<-merge(C,CPRD5,all.x=TRUE,all.y=FALSE) 
unique(C$data3)
#MANY UNITS
C<-C[!C$data3==97&!C$data3==1,]
#AVOID UNITS INDICATING PROPORTIONS OF Hba

#0=NA,56,57,,97,26
C$data2<-ifelse(C$data3==56,C$data2*10,C$data2)

C<-as.data.frame(C[,c(1,2,11)])
colnames(C)<- c("PatientID","event.date","Value")
C<-C[C$Value>=30 & C$Value<=260,]

columns=names(C[c(1,2)])
dots<-lapply(columns, as.symbol)
first <-C %>% 
group_by_(.dots=dots) %>%
summarise(Haemoglobin=mean(Value)) %>%
as.data.frame

indx1 <-neardate(crea.rep$PatientID, first$PatientID, crea.rep$event.date, first$event.date, 
                   best="prior")
crea.rep$Haemoglobin<-first[indx1, "Haemoglobin"]
crea.rep$HaemDateFlag<- ifelse(as.numeric(abs(crea.rep$event.date - first$event.date[indx1])) >30, 1, 0)
table(crea.rep$HaemDateFlag) #652
crea.rep$Haemoglobin<-unlist(crea.rep$Haemoglobin)

##############################################################################
save(crea.rep,file="crea.repongoing.rda")

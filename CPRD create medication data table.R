#Creates a table of relevant prescriptions per person by date.
setwd("/mnt/bmh01-rds/Peek_PERMIT/")
library(haven) #Reads dta format

doses<-read.table("common_dosages.txt",header=TRUE,sep="\t") #THIS IS THE CPRD PREDICTED DOSE INFORMATION FILE
#names=textid	text	daily_dose	dose_number	dose_unit	dose_frequency	dose_interval	choice_of_dose	dose_max_average	change_dose	dose_duration

product<-read.csv("productedited803.csv")#THIS IS THE MEDICATION LOOKUP
#names="CODE1"  "CODE2"        "CODE3"        "DESC"         "TYPE"
# "DOSE_PER_TAB" "FORMAT"       "FAMILY"
	
therapy<-read_dta("hf_cases_therapy.dta") #THIS IS THE PATIENT MEDICATION HISTORY
#names="patid"     "eventdate" "sysdate"   "consid"    "prodcode"  "staffid"
#"textid"    "bnfcode"   "qty"       "ndd"       "numdays"   "numpacks"
#"packtype"  "issueseq"

therapy<-therapy[therapy$prodcode %in% product$CODE1,]
product<-product[,c(1,4:8)]
names(product)<-c("prodcode","DESC","TYPE","DOSE_PER_TAB","FORMAT","FAMILY")
therapy<-merge(therapy,product,all.x=TRUE)
drugsCPRD<-merge(therapy,doses,all.x=TRUE)
save(drugsCPRD,file="drugsCPRD.rda")
load("crea.rep.rda")

drugsCPRD<-drugsCPRD[drugsCPRD$patid %in% crea.rep$PatientID,]
save(drugsCPRD,file="drugsCPRD.rda")

#CPRD patient IDs feature practise codes in the last 3 digits

library(stringr)
load("crea.rep.rda") #Primary table, 1 row per patient per date, summarising max creatinine per day
crea.rep$pracid<-str_sub(crea.rep$PatientID, start= -3)
a<-unique(crea.rep$pracid)
length(a) #403 practises total for cohort

prac<-read_dta("practice.dta")
prac<-prac[prac$pracid %in% a,]
length(unique(pats3$praccode))
prac<-prac[prac$region==2,]

length(unique(prac$pracid)) #56 northwest practises
save(prac,file="NWpractices.rda")
length(crea.rep$PatientID) #5609602
crea.rep<-crea.rep[!crea.rep$pracid %in% prac$pracid,]
length(crea.rep$PatientID) #5271263

#Add a random practise ID for splitting the data

pracid<-unique(crea.rep$pracid)
s1<-as.data.frame(pracid)
set.seed(111)
randompracID<-sample(1:length(pracid),length(pracid))
s2<-as.data.frame(randompracID)
s3<-cbind(s1,s2)
crea.rep<-merge(crea.rep,s3,all.x=TRUE)
save(crea.rep,file="crea.notnw")

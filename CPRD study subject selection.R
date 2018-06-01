require("haven")
require("gtools")
library("zoo")
library("plyr")
library("tidyverse")
library("data.table")
library("survival")
library("lubridate")
library("tidyr")
library("dplyr")
system.time(CPRD<-read_dta("hf_cases_therapy.dta")) #Medications
system.time(CPRD2<-read_dta("hf_cases_clinical.dta")) 
system.time(CPRD3<-read_dta("hf_cases_test.dta")) 
system.time(CPRD5<-read_dta("hf_cases_additional.dta")) 
#head(CPRD[CPRD$medcode=="5",]) #Only 2312 #References some dates of creatinine measures but no additional data
system.time(crea<-CPRD3[CPRD3$medcode=="5",]) #1579351 #This is the file with the creatinine values in
#load("creaongoing.rda")
#this is the file of Creatinine entries to be used as a reference 
#####################################################################################################
#SELECTING FOR HEART FAILURE
#SELECT PATIENTS WHO WERE 18 OR ABOVE AT THE TIME OF HEART FAILURE DIAGNOSIS

#Add hfage for selection

CPRD4<-read_dta("hf_cases_patient_practice_incidentHFdate.dta", encoding = 'latin1')
glimpse(CPRD4)

CPRD4$hfage<-(as.numeric(year(strptime(CPRD4$eventdate, format="%Y-%m-%d"))))-CPRD4$yob # calculate age at hf
CPRD4$hfage<-ifelse(as.numeric(month(strptime(CPRD4$eventdate, format="%Y-%m-%d")))>CPRD4$mob & CPRD4$mob>0,
                    CPRD4$hfage-1,
                    CPRD4$hfage)

colnames(CPRD4)
# [1] "patid"     "eventdate" "medcode"   "readterm"  "nvals"     "gender"    "yob"       "mob"       "marital"   "famnum"   
# [11] "frd"       "crd"       "regstat"   "reggap"    "internal"  "tod"       "toreason"  "deathdate" "hfage"

CPRD4<-CPRD4[,c(1:8,18,19)]

summary(CPRD4)

# patid             eventdate             medcode         readterm             nvals       gender            yob      
# Min.   :     1046   Min.   :1990-01-01   Min.   :   398   Length:283449      Min.   :1   Min.   :0.0000   Min.   :1882  
# 1st Qu.:  3371500   1st Qu.:1995-06-22   1st Qu.:   884   Class :character   1st Qu.:1   1st Qu.:0.0000   1st Qu.:1916  
# Median :  7439676   Median :2001-04-20   Median :   884   Mode  :character   Median :1   Median :1.0000   Median :1924  
# Mean   : 10263140   Mean   :2001-10-17   Mean   :  4287                      Mean   :1   Mean   :0.5117   Mean   :1926  
# 3rd Qu.: 14007227   3rd Qu.:2007-08-17   3rd Qu.:  2906                      3rd Qu.:1   3rd Qu.:1.0000   3rd Qu.:1933  
# Max.   :138566691   Max.   :2016-06-23   Max.   :108180                      Max.   :1   Max.   :2.0000   Max.   :2015  
# 
# mob              deathdate              hfage       
# Min.   : 0.000000   Min.   :1887-11-02   Min.   : -1.00  
# 1st Qu.: 0.000000   1st Qu.:1998-06-08   1st Qu.: 69.00  
# Median : 0.000000   Median :2003-10-27   Median : 78.00  
# Mean   : 0.009801   Mean   :2003-06-08   Mean   : 75.52  
# 3rd Qu.: 0.000000   3rd Qu.:2009-02-03   3rd Qu.: 84.00  
# Max.   :12.000000   Max.   :2016-06-22   Max.   :113.00  
# NA's   :107135    

# there is a patient with negative hfage. It will be removed when we only include patients with hfage>= 18

head(CPRD4[duplicated(CPRD4$patid),]) 
#None of the records are duplicated- each gives the hfdate based on 97 hf codes

CPRD4<-CPRD4[CPRD4$hfage>=18,] # only patients older than 18 at hf date
save(CPRD4,file="CPRD4.rda")
hfnames<-unique(CPRD4$patid)
length(hfnames) 
#282219

crea<-crea[crea$patid %in% hfnames,] # only crea of pat >= 18

##old
# colnames(CPRD4)
# #[1] "patid"     "eventdate" "medcode"   "readterm"  "nvals"     "gender"    "yob"       "mob"       "deathdate" "hfage"    
# CPRD4<-CPRD4[,c(1,2,6:10)]
##equivalent
CPRD4 <- CPRD4 %>%
  select(patid, eventdate, gender:hfage)

CPRD4$hfdate<-CPRD4$eventdate

##old
# colnames(CPRD4)
#"patid"     "eventdate" "gender"    "yob"       "mob"       "deathdate" "hfage"     "hfdate"   
# CPRD4<-CPRD4[,c(1,3:8)]
##equivalent
CPRD4 <- CPRD4 %>%
  select(-eventdate)

CPRD4<-as.data.frame(CPRD4)
crea<-as.data.frame(crea)
crea<-merge(crea,CPRD4,all.x=TRUE,all.y=FALSE)

summary(crea)

save(crea, file = "CPRD_crea.Rdata") 

##################################################################################################### 
#START WITH A SKELETON OF CREATININE VALUES THEN ADD ON TO IT.

##Remove NAs then order and select the mean value per day
crea<-crea[!is.na(crea$data2),] # remove NAs
crea<-crea[order(-crea$data2),] # why in decreasing order?!

#keep only mean value for each day
smalltab<-crea %>%
  #group_by_(.dots=c("patid","eventdate")) %>%
   group_by(patid, eventdate) %>%
    summarize(Creatinine = mean(data2)) %>%
      ungroup()%>%
        as.data.frame()

crea<-merge(crea,as.data.frame(smalltab),all.x=TRUE) %>%  # why are we keeping also the duplicates?
  distinct(patid, eventdate, Creatinine, .keep_all = TRUE)

summary(crea$Creatinine)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       0      84     103     116     131   10100   11608 


crea <- crea %>%
  filter(!is.na(Creatinine)) # there are still NA's let's remove them

summary(crea$Creatinine)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0      84     103     116     131   10100 

save(crea, file = "CPRD_crea2.rda") 

#######################################################################################################
#CHECK UNITS IN CASE VALUES NEED CONVERTING

#C<-unique(crea$data3) 
#Should be 142(umol/L). Lookups for units are in the text file 'SUM.txt'
#write.csv(C,file="SUMcrea.csv")
#HAND ANNOTATE WITH CONVERSION FACTORS

C<-read.csv("SUMcrea.csv")
colnames(C)
#[1] "Code" "Specimen.Unit.Of.Measure" "CodeUnits" "Times"   
colnames (C)[1]<-"data3"
#crea<-merge(crea,C[,c(1,3,4)],all.x=TRUE,all.y=FALSE)
crea<-merge(crea,C[,c("data3","CodeUnits","Times")],all.x=TRUE,all.y=FALSE)
save(crea, file = "CPRD_crea2.rda") 
#BIND ON UNITS OF MEASUREMENT

#CONVERT PROBLEMATIC UNITS
#crea$data2<-ifelse(!crea$CodeUnits=="umol/L",(crea$data2*crea$Times),crea$data2)
#MMOL/L UNITS SEEM TO BE MISNAMED THROUGHOUGH-ALL ARE IN RANGE IF UMOL/L ASSUMED 
#WE LOSE TOO MUCH DATA IF WE CONVERT THEM THEN OMIT THESE.

unique(droplevels(crea$CodeUnits))
# check distribution for different units
crea %>%
  group_by(CodeUnits) %>%
    summarise(n = n(),
              min = min(Creatinine),
              first = quantile(Creatinine, 0.25),
              mean = mean(Creatinine),
              third = quantile(Creatinine, 0.75),
              max = max(Creatinine))

# # A tibble: 11 x 7
# CodeUnits       n   min  first   mean  third    max
# <fct>         <int> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
# 1 ""              1  94    94     94     94       94 
# 2 mg/dL          11   0.9   1.10   9.46   1.35    92 
# 3 mg/L            1   6     6      6      6        6 
# 4 mmol/L      47042   0    86    115.   129      999 
# 5 mol/L        7670   4.5  81    114.   132      853 
# 6 ng/L            2  82    87     92     97      102 
# 7 nmol/L          6  97   136.   156.   190.     215 
# 8 pmol/L          6  60   113.   117.   133.     150 
# 9 ug/L            5   3    75    107.   162      170 
# 10 umol/L    1422277   0    84    116.   131    10096.
# 11 <NA>          287   0.5  81    114.   134.     418 

# There seems to be only three main Units used. Umol/L is the main one. However it seems that independently from the Units the data is harmonised. Most units have similar distribution.

#CUTOFF IMPLAUSIBLE VALUES
crea<-droplevels(crea[crea$data2>=20&crea$data2<=3000,])
nrow(crea)#[1] 1474502

save(crea, file = "CPRD_crea2.rda")
length(levels(as.factor(crea$patid)))
#[1] 120635

########################################################################################################
#Add birth and death and basic formulaic variables
crea$Dead<-ifelse(!is.na(crea$deathdate),1,0)
#The death dates are not reliable, many of them are before 1950 and 100 are before 1980.

crea$Age<-(as.numeric(year(strptime(crea$eventdate, format="%Y-%m-%d"))))-crea$yob
# crea$Age<-ifelse(as.numeric(month(strptime(crea$eventdate, format="%Y-%m-%d")))>crea$mob & crea$mob>0, # this is never true
#                  crea$Age-1,
#                  crea$Age)

crea <- crea %>%
  filter(Age > 0) # exclude impossible values

save(crea, file = "CPRD_crea2.rda")
crea$log_CREA<-log(crea$Creatinine)

colnames(crea)
# [1] "data3"      "patid"      "eventdate"  "sysdate"    "constype"   "consid"     "medcode"    "staffid"    "textid"    
# [10] "enttype"    "data1"      "data2"      "data4"      "data5"      "data6"      "data7"      "data8"      "gender"    
# [19] "yob"        "mob"        "deathdate"  "hfage"      "hfdate"     "Creatinine" "CodeUnits"  "Times"      "Dead"      
# [28] "Age"        "log_CREA"
colnames(crea)[2] <- "PatientID"
colnames(crea)[3] <- "event.date"
colnames(crea)[18] <- "Gender"
crea<-crea[,c(2,3,18,21:24,26:28)]
save(crea, file = "CPRD_crea2.rda")
########################################################################################################

#Add demographic variables from lookup tables
#CPRD2<-read_dta("hf_cases_clinical.dta")  

#CODE ETHNICITY
eth<-read.table("ethnicitycprd.csv",header=TRUE,sep=",")
colnames(eth)
#[1] "medcode"  "CLINCODE" "OTHER"    "RUBRIC"   "Category"
eth<-eth[,c("medcode","Category")]
#WHICH TABLE ARE THE ETH MEDCODES IN?
CPRD2<-merge(CPRD2,eth,by.x="medcode",by.y="medcode",all=TRUE)
CPRD2<-CPRD2[!is.na(CPRD2$Category),]

table(CPRD2$Category)
# 1   1.1   1.2   1.3   1.4     2   2.1   2.2   2.3   2.4     3   3.1   3.2   3.3   3.4     4   4.1   4.2   4.3   4.4     5 
# 6982 43774   866  2010     5    63    54    31    27    57    19   930   355    72   419   110   491   354    74    15     4 
# 5.1   5.2     6     7 
# 84   128  2588  1584 
colnames(CPRD2)
# [1] "medcode"   "patid"     "eventdate" "sysdate"   "constype"  "consid"    "staffid"   "textid"    "episode"   "enttype"  
# [11] "adid"      "Category" 
CPRD2<-CPRD2[,c("patid","Category")]
CPRD2<-unique(CPRD2)
head(CPRD2[duplicated(CPRD2),])
#THERE ARE NO PATIENTS WITH CONFLICTING ETHNICITY CODES

colnames(CPRD2)[1] <- "PatientID"
colnames(CPRD2)[2] <- "Ethnicity"
crea<-merge(crea,CPRD2,all.x=TRUE)

summary(crea)

#Add LSOA
IMD<-read.csv("patient_imd2010_16_241RMnA.txt",sep="\t")
colnames(IMD)
#[1] "patid"     "pracid"    "imd2010_5"
IMD<-IMD[,c(1,3)]
colnames(IMD)<-c("PatientID","IMD_Decile2010")
crea<-merge(crea,IMD,all.x=TRUE)

summary(crea)

#CHECK ALSO FOR 23955 generic code x1
#DRAW INFORMATION ON 23955 FROM LOOKUP TABLE
CPRD2<-read_dta("hf_cases_clinical.dta")  
C2<-CPRD2[CPRD2$medcode=="23955",]
table(C2$adid) #Not linked to text

#################################################################################################################
#COHORT SELECTION
# #LIMIT TO PATIENTS WITH AT LEAST 2 (POST 2008?) CREATININE TEST VALUES
# #crea<-crea[as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d")))>=2008,]
# table(crea$PatientID) < 2 -> rare  
# rownames(as.matrix(rare)) -> ids
# table(rare)
# crea[!(crea$PatientID %in% ids[rare]),] -> crea.rep	
# levels(unique(as.factor(crea.rep$PatientID))) # 198543 adult hf patients have 2 or more creatinine tests
# length(crea.rep$PatientID) #1579775 rows remaining
crea.tmp <- crea[as.numeric(year(strptime(crea$event.date, format="%Y-%m-%d")))>=2008,]

ids <- crea.tmp %>%
  group_by(PatientID) %>%
    count() %>%
      ungroup() %>%
        filter(n > 1)

# keep only patients with two creatinine after 2008
crea.rep <- crea[(crea$PatientID %in% ids$PatientID),]
CPRD2 <- CPRD2[(CPRD2$patid %in% ids$PatientID),]

##########################################################################

#SELECT PATIENTS WHICH HAVE AT LEAST 2 TESTS AND AT LEAST 2 YEARS DATA after 2008
#FIND MIN AND MAX DATES PER PATIENT
# x1[(which(x1$range<2)),1] -> range_short_ids    # define exclusion range as 2 years
# crea[-which(crea$PatientID %in% range_short_ids),]->crea.rep 
# length(unique(crea.rep$PatientID))
# save(crea.rep,file="crea.rep.rda")


x1<- CPRD2 %>% 
  filter(eventdate >= as.Date("2008-01-01")) %>% 
    group_by(patid) %>%
    summarize(start=min(eventdate),
              end= max(eventdate)) %>%
      ungroup() %>%
        mutate(range = as.numeric(as.character(end-start))/365) %>%
          filter(range >= 2)

crea.rep <- crea.rep %>%
  filter(PatientID %in% x1$patid)

summary(crea.rep)
# PatientID          event.date             Gender         deathdate              hfage            hfdate          
# Min.   :    1268   Min.   :1935-04-27   Min.   :0.0000   Min.   :1913-07-24   Min.   : 18.00   Min.   :1990-01-01  
# 1st Qu.: 3403670   1st Qu.:2006-05-04   1st Qu.:0.0000   1st Qu.:2011-07-27   1st Qu.: 67.00   1st Qu.:2004-04-23  
# Median : 7669028   Median :2009-07-20   Median :0.0000   Median :2013-01-01   Median : 75.00   Median :2009-02-09  
# Mean   :10707875   Mean   :2009-02-16   Mean   :0.4515   Mean   :2012-12-16   Mean   : 73.61   Mean   :2007-11-24  
# 3rd Qu.:14923047   3rd Qu.:2012-04-27   3rd Qu.:1.0000   3rd Qu.:2014-06-26   3rd Qu.: 82.00   3rd Qu.:2012-05-10  
# Max.   :92830229   Max.   :2016-06-24   Max.   :1.0000   Max.   :2016-06-22   Max.   :107.00   Max.   :2016-06-23  
# NA's   :702268                                            
# Creatinine          Times              Dead             Age           Ethnicity      IMD_Decile2010 
# Min.   :   20.0   Min.   :      0   Min.   :0.0000   Min.   :  7.00   Min.   :1.0      Min.   :1.000  
# 1st Qu.:   82.0   1st Qu.:      1   1st Qu.:0.0000   1st Qu.: 68.00   1st Qu.:1.1      1st Qu.:2.000  
# Median :   99.0   Median :      1   Median :0.0000   Median : 76.00   Median :1.1      Median :3.000  
# Mean   :  111.6   Mean   :   4532   Mean   :0.3526   Mean   : 74.82   Mean   :1.6      Mean   :2.877  
# 3rd Qu.:  125.0   3rd Qu.:      1   3rd Qu.:1.0000   3rd Qu.: 83.00   3rd Qu.:1.1      3rd Qu.:4.000  
# Max.   :10095.5   Max.   :1000000   Max.   :1.0000   Max.   :109.00   Max.   :7.0      Max.   :5.000  
# NA's   :446774   NA's   :485 
#  
save(crea,file="CPRD_crea.repongoing.rda")



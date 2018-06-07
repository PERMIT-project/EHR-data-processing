library("stringr")
library("lubridate")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("tidyverse")

setwd("~/Peek_PERMIT")

load("sir.data2yrsall.rda") #Full EHR extract
load("SIR_crea.repongoing.rda") #Primary table with 1 row per patient per date summarising the mean creatinine value per day, with pathology results
inst<-read.csv("inst051217.csv") #Lookup regex file of unique textual precription instructions paired with implications for dose, number etc
codes1<-read.csv("SIRdrugs.csv") #Lookup of medication type, family, active ingredient etc

#Subset extract if required to enable faster processing
sir.data <- sir.data %>%
  filter(event.date >= as.Date("2007-01-01") & event.date <= as.Date("2017-09-01")) %>%
    filter(ReadCode %in% codes1$CODE & PatientID %in% crea.rep$PatientID) %>%
      droplevels()
  
#Coerce date fields to date format if needed
sir.data$EntryDate<-as.Date(as.character(sir.data$EntryDate),format="%Y%m%d")

inst$DESCRIPTION<-gsub('[[:punct:]]','',inst$DESCRIPTION) #REMOVE PUNCTUATION
sir.data$CodeUnits<-gsub('[[:punct:]]','',sir.data$CodeUnits) #REMOVE PUNCTUATION
inst$DESCRIPTION<-tolower(inst$DESCRIPTION) #LOWER CASE
sir.data$CodeUnits<-tolower(sir.data$CodeUnits)

#REMOVE ANY DUPLICATE PRECRIPTION ENTRIES
sub <- sir.data %>% 
        select(PatientID,ReadCode,CodeValue, CodeUnits,EntryDate) %>%
          distinct() %>%
            rename(DESCRIPTION = CodeUnits) #ENSURE FIELD NAMES MATCH BETWEEN TABLES


#REPLACE WRITTEN NUMBERS WITH NUMBERS 1-6 IN PATIENT DATA (here already complete in instruction data lookup)

sub$DESCRIPTION<-gsub("one", "1", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("two", "2", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("three", "3", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("four", "4", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("five", "5", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("six", "6", sub$DESCRIPTION) 

sub$DESCRIPTION<-tolower(sub$DESCRIPTION)#LOWER CASE
sub$DESCRIPTION<-gsub(" ", "", sub$DESCRIPTION, fixed = TRUE)  #REMOVE SPACES FROM INSTRUCTION STRINGS (This step already taken in instruction data lookup)
inst<-inst %>% 
        distinct(DESCRIPTION,.keep_all = TRUE)
#MAKE SURE NO DUPLICATE LINES IN THE LOOKUP TABLE THAT CAN LEAD TO NAs IN THE FINAL TABLE
#############################################################################################################
#JOIN THE INSTRUCTIONS AND RELATED FIELDS ONTO THE MAIN FILE

sub$DESCRIPTION<-gsub("#", "", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub(",", "", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub(":", "", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("[.]", "", sub$DESCRIPTION) 
sub$DESCRIPTION<-str_replace(sub$DESCRIPTION, "(ip.*)", "")
sub$DESCRIPTION<-gsub("[(]", "", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("[)]", "", sub$DESCRIPTION) 
inst$DESCRIPTION<-gsub("[)]", "", inst$DESCRIPTION) 
inst$DESCRIPTION<-gsub("[(]", "", inst$DESCRIPTION) 

ESS<-unique(sub$DESCRIPTION[!(sub$DESCRIPTION %in% inst$DESCRIPTION)])
ESS %>%
  length() #2
sub %>% filter(!(DESCRIPTION %in% inst$DESCRIPTION)) %>%
  nrow() #[1] 29

write.csv(ESS,file="EXTRADESCS.csv") #OUTPUT ANY NON PARSING DESCRIPTIONS. ADD TO THE REGEX TABLE, ANNOTATE AND RERUN

##################################################################################
#MARK PRESCRIPTIONS WHICH ARE INTENDED TO BE EXTRA TABLETS TO ADD TO AN EXISTING DOSE OF THE SAME DRUG.

EX<-sub$DESCRIPTION[grep("extra",sub$DESCRIPTION)]
AD<-sub$DESCRIPTION[grep("additionto",sub$DESCRIPTION)]
AD2<-sub$DESCRIPTION[grep("additional",sub$DESCRIPTION)]
sub$EXTRA<-ifelse(sub$DESCRIPTION %in% AD | sub$DESCRIPTION %in% AD2 |sub$DESCRIPTION %in% EX,1,0)
#'EXTRA' MARKS PRESCRIPTIONS THAT ARE ADDITIONS OF MORE TO THE SAME DRUG, 
#OFTEN SUPPLEMENTING BOXED MEDICATION (E.G. 'TAKE AN EXTRA TABLET EVERY MORNING WITH THE ONE IN YOUR VENALINK').
############################################################################# 
#QUANTIFY MISSING PRESCRIPTION DATA

#sub<-merge(sub,inst,all.x=TRUE)
sub <- sub %>%
  left_join(inst, by = "DESCRIPTION")

str(sub)
# 'data.frame':  1129215 obs. of  6 variables:
# $ PatientID  : int  1 1 1 1 1 1 1 1 1 1 ...
# $ ReadCode   : Factor w/ 641 levels "14L..","66R5.",..: 279 279 279 279 279 279 279 279 279 279 ...
# $ CodeValue  : Factor w/ 157 levels "","0","0.5","000000000P",..: 22 22 34 34 34 34 34 61 61 69 ...
# $ DESCRIPTION: chr  "tablets" "tablets" "tablets" "tablets" ...
# $ EntryDate  : Date, format: "2007-03-26" "2007-04-30" "2007-08-02" "2007-09-18" ...
# $ EXTRA      : num  0 0 0 0 0 0 0 0 0 0 ...


#here with the last join we have the info that we need to spot equivalent records for which the only difference is how the prescription is spelled. e.g. take2onthefirstdaythen1eachday and take2todaythen1eachday can be considered as a duplicated record if prescribed for the same patient on the same day for the same drug

sub <- sub %>% 
  distinct(PatientID, ReadCode, CodeValue, EntryDate, EXTRA, .keep_all = TRUE)

length(sub$DESCRIPTION[sub$DESCRIPTION==""])
#[1] 4162

sub <- sub %>%
  rename(CODE = ReadCode)  #RENAME FIELDS IF NEEDED FOR LATER MERGING

#JOIN ON  DOSAGE DATA
subs <- sub %>%
  filter(CODE %in% codes1$CODE) %>%
    left_join(codes1, by = "CODE")

# for some codes there are multiple rows in codes1 therefore nrow(subs)>nrow(sub)

########################################################################### 
#CALCULATE MISSING DATA WHERE POSSIBLE BASED ON PRESENT FIELDS
subs$DAILY_DOSE<-as.numeric(subs$DAILY_DOSE)

subs$DOSE_PER_TAB<-as.numeric(subs$DOSE_PER_TAB)

subs$TABLETS_PER_DAY<-as.numeric(subs$TABLETS_PER_DAY)

summary(subs$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.0    24.0    50.0    46.8    71.0    91.0 1251520 

#mostly of daily_dose is NAs, let's try to use the data in the other columns to infer it

subs$DAILY_DOSE<-ifelse(is.na(subs$DAILY_DOSE)&
                          !is.na(subs$TABLETS_PER_DAY)&
                          !is.na(subs$DOSE_PER_TAB),
                        subs$TABLETS_PER_DAY*subs$DOSE_PER_TAB,
                        subs$DAILY_DOSE)

summary(subs$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    10.0    18.0    23.4    35.0  1176.0  614646 

summary(subs$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.000   1.000   1.216   1.000  50.000    3322 
# only a minority is NA, let's try to infer it applying an inverse formula

subs$TABLETS_PER_DAY<-ifelse(is.na(subs$TABLETS_PER_DAY)&
                               !is.na(subs$DAILY_DOSE)&
                               !is.na(subs$DOSE_PER_TAB),
                             subs$DAILY_DOSE/subs$DOSE_PER_TAB,
                             subs$TABLETS_PER_DAY)
summary(subs$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.000   1.000   1.217   1.000  50.000    2249 

summary(subs$EntryDate[is.na(subs$DAILY_DOSE)])
#     Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2007-01-01" "2009-10-20" "2011-12-19" "2011-12-23" "2014-03-17" "2017-06-20" 

#FIND THE MEDIAN DOSE FOR EACH DRUG AND USE THIS IF MISSING
b<-subs[,c("DAILY_DOSE","TYPE")]
b<-na.omit(b)
#b$DAILY_DOSE<-as.numeric(b$DAILY_DOSE)
b2<-b %>% 
  group_by(TYPE) %>% 
    summarise(MEDIAN_DOSE = median(DAILY_DOSE, na.rm = TRUE)) %>% 
      as.data.frame

# append median_dose in the main dataset
subs <- subs %>%
  left_join(b2, by = "TYPE")

subs$DAILY_DOSE<-ifelse(is.na(subs$DAILY_DOSE), # substitute if missing
                        subs$MEDIAN_DOSE,
                        subs$DAILY_DOSE)

############################################################################
#DIVIDE INTO MULTIPLE ROWS IF THE DOSE CHANGES OVER TIME
#THIS CODE OVERWRITES SO BE CAREFUL TO GO BACK TO THE INITIAL CONSTRUCTION OF SUBS IF YOU WANT TO RUN IT AGAIN TO AVOID REPETITIVELY CREATING NEW ROWS.
#THEN should contain the number of dose changes (i.e. the number of extra rows you need, so most rows should equal zero.)

#######DEFINE THOSE PRESCRIPTINS WITH PROGRESSIVE DOSING
subs$THEN<-ifelse(is.na(subs$THEN),0,subs$THEN)

subs$THEN <- subs$THEN +1 # to avoid the non-replication of zeros
subs <- expandRows(subs, "THEN", drop = FALSE) #Create a copied row for each changing dosage
subs$THEN <- subs$THEN - 1 # not sure if there is reference to the 0/1 values later but in this way we can bring it back to it
subt<-subs[subs$THEN>0,] # select prescriptions with progressive dosage
glimpse(subt)

# 'data.frame':  8882 obs. of  21 variables:
#   $ PatientID      : int  20 20 20 20 20 20 20 20 20 20 ...
# $ CODE           : chr  "e758." "e758." "e758." "e758." ...
# $ CodeValue      : Factor w/ 157 levels "","0","0.5","000000000P",..: 130 130 130 130 130 130 130 130 130 130 ...
# $ DESCRIPTION    : chr  "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" ...
# $ EntryDate      : Date, format: "2011-03-21" "2011-03-21" "2011-05-27" "2011-05-27" ...
# $ EXTRA          : num  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  2 2 2 2 2 2 2 2 2 2 ...
# $ DAILY_DOSE     : num  5 5 5 5 5 5 5 5 5 5 ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 42 levels "Amiodarone","Amlodipine",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAYS           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ NUM2           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1152 levels "ACCUPRO 10mg-28CP tabs",..: 356 356 356 356 356 356 356 356 356 356 ...
# $ TYPE           : Factor w/ 98 levels "Aceclofenac",..: 30 30 30 30 30 30 30 30 30 30 ...
# $ DOSE_PER_TAB   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ FAMILY         : Factor w/ 9 levels "ACEI","ALD_ANT",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...
# $ MEDIAN_DOSE    : num  5 5 5 5 5 5 5 5 5 5 ...

subnt<-subs[subs$THEN == 0,] # select single prescriptions
str(subnt)

# 'data.frame':  1260109 obs. of  21 variables:
#   $ PatientID      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ CODE           : chr  "META3900" "META3900" "META3900" "META3900" ...
# $ CodeValue      : Factor w/ 157 levels "","0","0.5","000000000P",..: 22 22 34 34 34 34 34 61 61 69 ...
# $ DESCRIPTION    : chr  "tablets" "tablets" "tablets" "tablets" ...
# $ EntryDate      : Date, format: "2007-03-26" "2007-04-30" "2007-08-02" "2007-09-18" ...
# $ EXTRA          : num  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAILY_DOSE     : num  22 22 22 22 22 22 22 22 22 22 ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 42 levels "Amiodarone","Amlodipine",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ DAYS           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ NUM2           : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1152 levels "ACCUPRO 10mg-28CP tabs",..: 689 689 689 689 689 689 689 689 689 689 ...
# $ TYPE           : Factor w/ 98 levels "Aceclofenac",..: 61 61 61 61 61 61 61 61 61 61 ...
# $ DOSE_PER_TAB   : num  22 22 22 22 22 22 22 22 22 22 ...
# $ FAMILY         : Factor w/ 9 levels "ACEI","ALD_ANT",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...
# $ MEDIAN_DOSE    : num  22 22 22 22 22 22 22 22 22 22 ...
############################################################################

#EDIT THE DOSES FOR THE REPLICATE ROWS (FOLLOWING DOSE CHANGE)
subt$DAILY_DOSE<-ifelse(duplicated(subt$PatientID)&
                          duplicated(subt$EntryDate)&
                          duplicated(subt$TYPE), 
                        subt$DOSE2,
                        subt$DAILY_DOSE)

summary(subt$DAILY_DOSE)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.280    5.000    5.000    8.249    5.000 1000.000     6188 

subt$TABLETS_PER_DAY<-ifelse(duplicated(subt$PatientID)&
                               duplicated(subt$EntryDate)&
                               duplicated(subt$TYPE), #same here
                             as.numeric(as.character(subt$NUM2)),
                             subt$TABLETS_PER_DAY)

summary(subt$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.140   1.000   1.000   1.299   2.000   8.000      19 

subt$n<-(subt$TABLETS_PER_DAY*as.numeric(subt$DAYS)) # total tablets prescribred

summary(subt$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.280   1.000   1.000   1.739   2.000  84.000      19 

subt$C1<-ifelse((duplicated(subt$PatientID)&
                   duplicated(subt$EntryDate)&
                   duplicated(subt$TYPE)), # check if duplicated type
                1,
                0) 
table(subt$C1)
# 0    1 
# 2612 6270 

subt$CodeValue<-ifelse(subt$C1==1,
                       as.integer(as.numeric(as.character(subt$CodeValue))-as.numeric(as.character(subt$n))),
                       paste(subt$CodeValue))#For the second entry, recalculate the prescription minus what was used up whilst on the original dosage
subt$EntryDateb<-ifelse(subt$C1==1,
                        subt$EntryDate+subt$DAYS,
                        subt$EntryDate) # adjust date to plausible prescription date

subt$EntryDate<-as.Date(subt$EntryDateb,origin=(subt$EntryDate[1]-subt$EntryDateb[1]))

subt<-subt[,c(1:19,21)]
subnt<-subnt[,c(1:19,21)]

meddata<-rbind(subt,subnt)
str(meddata)

# 'data.frame':  1268991 obs. of  20 variables:
#   $ PatientID      : int  20 20 20 20 20 20 20 20 20 20 ...
# $ CODE           : chr  "e758." "e758." "e758." "e758." ...
# $ CodeValue      : chr  "8" "7" "8" "7" ...
# $ DESCRIPTION    : chr  "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" ...
# $ EntryDate      : Date, format: "2011-03-21" "2011-03-22" "2011-05-27" "2011-05-28" ...
# $ EXTRA          : num  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  2 1 2 1 2 1 2 1 2 1 ...
# $ DAILY_DOSE     : num  5 NA 5 NA 5 NA 5 NA 5 NA ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 42 levels "Amiodarone","Amlodipine",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAYS           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ NUM2           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1152 levels "ACCUPRO 10mg-28CP tabs",..: 356 356 356 356 356 356 356 356 356 356 ...
# $ TYPE           : Factor w/ 98 levels "Aceclofenac",..: 30 30 30 30 30 30 30 30 30 30 ...
# $ DOSE_PER_TAB   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ FAMILY         : Factor w/ 9 levels "ACEI","ALD_ANT",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ MEDIAN_DOSE    : num  5 5 5 5 5 5 5 5 5 5 ...

############################################################################
#ASSIGN DATE OF END OF PRESCRIPTION

m<-(as.numeric(meddata$CodeValue)/as.numeric(meddata$TABLETS_PER_DAY))
meddata$END_DATE<-ifelse(!is.na(meddata$TABLETS_PER_DAY)&
                           !is.na(subs$CodeValue),
                         meddata$EntryDate+as.difftime(m, unit="days"),
                         NA)
summary(meddata$END_DATE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 13520   14510   15290     Inf   16120     Inf    9503 
meddata$END_DATE<-as.Date(meddata$END_DATE,origin="1970-01-01")
head(meddata)
summary(meddata$END_DATE)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2007-01-05" "2009-09-25" "2011-11-11"           NA "2014-02-17"           NA       "9503" 

save(meddata,file="SIR_PERMITmeddata28.rda")

############################################################################
#STOP/REPLACE INSTRUCTIONS- ROWS WITH INSTRUCTIONS WHICH RELATE TO THE REPLACEMENT OF DRUGS WITH
#A NEW DRUG OR NEW LEVEL OF DOSE HAVE DATA WHICH HAS BEEN ENTERED IN THE ALT_OTHER_MEDS COLUMN 
#(Short for alterations to other medications)

load("SIR_PERMITmeddata28.rda")

meddata<-meddata %>% 
  distinct()

meddata$REP<-ifelse(meddata$REP == "Same",
                    paste(meddata$TYPE),
                    paste(meddata$REP))

#REP refers to the replacement of one prescribed drug with an ongoing prescription with another drug (e.g. due to adverse reaction)
#"Same" in the REP column means that an ongoing prescription for a drug is being replaced by a different instruction or dosage for the same drug"
#e.g. "Take instead of the 10mg tablets"
#So, if REP is "same", we paste the drug name from TYPE. If REP lists another drug, we action changes to the end date of the last prescription of that drug

meddata$REP<-as.factor(meddata$REP)

table(meddata$REP, useNA = "ifany")

#Check there are no situations in which 2 or more different drugs are simultaneously being replaced
head(meddata$REP2[!(meddata$REP2 %in% meddata$REP)]) #ALL THOSE IN REP2 ARE ALSO IN REP

#Make a small subset table for processing purposes of ongoing drug prescriptions which are being replaced
#before their end dates so we can edit these rows
#restrict to drugs of interest to the study
r<-paste(unique(meddata$TYPE))

meddata$REP<-ifelse(meddata$REP %in% r & !is.na(meddata$REP),
                    paste(meddata$REP),
                    NA)

meddata$REP2<-ifelse(meddata$REP2 %in% r& !is.na(meddata$REP2),
                     paste(meddata$REP2),
                     NA) 
meddata$ALT_OTHER_MEDS<-ifelse(is.na(meddata$REP),
                               NA,
                               meddata$ALT_OTHER_MEDS)
#Replace irrelevant drug changes in ALT_OTHER_MEDS

#NEAREST DATE MATCH OF THE REPLACEMENT INSTRUCTION TO THE LAST PRIOR ENTRY OF THE DRUG BEING STOPPED/REPLACED
#USING SIMILAR LOGIC TO THE CONDITIONAL VARIABLE EXTRACTION

#Speed things up by subsetting meddata to those drugs listed in REP
#(There are only a few, typically those causing interaction problems)

coda<-meddata[!is.na(meddata$REP),]
stops<-meddata %>% 
  filter(TYPE %in% meddata$REP) %>% 
    select(PatientID,EntryDate,END_DATE,TYPE)
#Stops is a subset table of prescriptions of drugs listed in REP
#Coda is a subset of rows instructing replacement or stop of a drug

columns=names(stops[c(1,2,4)]) #This should be PatientID, EntryDate and TYPE

dots<-lapply(columns, as.symbol)
first <-stops %>% 
group_by_(.dots=dots) %>%
as.data.frame 

library(survival)

#Create a new end date column to reflect truncation of the prescription based on near date match to a prior stop command
#Which occurs after the start and before the original prescription end date

first$NEWENDDATE<-NA
for (i in 1:length(unique(first$TYPE))){
  
codab<- coda[coda$REP==first$TYPE[i],]
indx<- neardate(first$PatientID, 
                codab$PatientID, 
                first$EntryDate,
                codab$EntryDate,
                best="after")

first$NEWENDDATE<-(ifelse(first$TYPE==first$TYPE[i],
                          codab[indx,"EntryDate"],
                          first$NEWENDDATE))

}

#For each row prescribing a drug with some stop codes listed in our stops table, see if there is a stop recorded in REP
#after the prescription date

first$NEWENDDATE<-as.Date(first$NEWENDDATE,origin="1970-01-01")
head(first[!is.na(first$NEWENDDATE),])

first<-first[!is.na(first$NEWENDDATE)&
               first$NEWENDDATE>first$EntryDate &
               first$NEWENDDATE<first$END_DATE,
             c("PatientID","EntryDate","END_DATE","TYPE","NEWENDDATE")]

#Be careful with neardate- we have told it to look for entries after the EntryDate but if it does not find one
#it will look for one before EntryDate. Here we confirm that the stop instruction fell between the entry and original end date

length(first$PatientID) #Count how many prescriptions are affected

meddata <- merge(meddata,
                 first,
                 all.x=TRUE)
#Each affected entry now has and ENDDATE and NEWENDDATE. We take the earlier of the two and replace ENDDATE then proceed as
#with all other unaffected rows.

meddata$END_DATE<-ifelse(!is.na(meddata$NEWENDDATE)&
                           meddata$END_DATE>meddata$NEWENDDATE &
                           meddata$EntryDate<meddata$NEWENDDATE,
                         meddata$NEWENDDATE,
                         meddata$END_DATE)

meddata$END_DATE<-as.Date(meddata$END_DATE,origin="1970-01-01")
#save(meddata,file="PERMITmeddata28.rda") #If you want to overwrite at this point

#IF YOU HAVE ENTRIES IN REP2 AT THIS POINT YOU NEED TO REPEAT THE PROCESS AGAIN FOR REP2 AS FOR REP. 
#AND SO ON FOR REP3 ETC IF YOU NEED THIS IN YOUR SPECIFIC CONTEXT.
####################################################################################
#DEAL WITH 'EXTRAS'
ex<-meddata[meddata$EXTRA==1,]
subx<-meddata[meddata$EXTRA==0,]
ex<-ex[ex$TYPE %in% subx$TYPE & ex$PatientID %in% subx$PatientID & ex$EntryDate>=subx$EntryDate & ex$END_DATE<subx$END_DATE,]
ex[!is.na(ex$TYPE),] #2 specific entries are impacted
#head(subx)
#smalltab<-ex[,c("PatientID","TYPE","EntryDate","END_DATE","DAILY_DOSE")]
#columns=names(smalltab[c(1:3)])
#dots<-lapply(columns, as.symbol)
#firstU <-smalltab %>% 
#group_by_(.dots=dots) %>%
#as.data.frame

#firstU$AddDate<-NA
#firstU$AddDose<-NA
#for (i in 1:unique(as.factor(firstU$TYPE))){
#exb<-ex[ex$TYPE==firstU$TYPE[i],]
#indx<-neardate(firstU$PatientID, subx$PatientID, firstU$EntryDate,subx$EntryDate,best="after")
#firstU$AddDate<-(ifelse(firstU$TYPE==firstU$TYPE[i],subx[indx,"EntryDate"],firstU$AddDate))
#firstU$AddDose<-(ifelse(firstU$TYPE==firstU$TYPE[i],subx[indx,"DAILY_DOSE"],firstU$AddDose))
#}
#firstU$AddDate<-as.Date(firstU$AddDate,origin="1970-01-01")
#head(firstU[!is.na(firstU$AddDate),])
#firstU<-firstU[!is.na(firstU$AddDate)&firstU$AddDate>=firstU$EntryDate&firstU$AddDate<firstU$END_DATE,c("PatientID","EntryDate","END_DATE","TYPE","AddDate","AddDose")]
#length(firstU$PatientID)#1252 prescriptions are affected
#names(firstU)

##################################################################### 

#CONVERT VOLUME DOSAGES FOR LIQUID MEDS. OMIT WATER TO AVOID PICKING UP SOLID DOSE MEDS INSTRUCTED TO BE DISSOLVED IN WATER.
meddata$CodeValue <- ifelse(grepl("ml",meddata$DESCRIPTION)&
                              !grepl("water",meddata$DESCRIPTION),
                            as.numeric(meddata$CodeValue)/5,
                            as.numeric(meddata$CodeValue))

meddata<-unique(meddata)
meddata$DAILY_DOSE<-signif(meddata$DAILY_DOSE,digits=2)
meddata$FAMILY<-ifelse(meddata$TYPE=="Doxycycline",paste("Antimicrobial"),paste(meddata$FAMILY))
meddata$FAMILY<-ifelse(meddata$TYPE=="Chloretracycline",paste("Chlortetracycline"),paste(meddata$FAMILY))
meddata$EntryDate<-as.Date(meddata$EntryDate,format="%d/%m/%Y")
meddata$END_DATE<-as.Date(meddata$END_DATE,format="%d/%m/%Y")

#Correct any analagous types
meddata$TYPE<-ifelse(meddata$TYPE=="Hydrochlorothiazide",paste("Hydrochlorthiazide"),paste(meddata$TYPE))
meddata$TYPE<-ifelse(meddata$TYPE=="Trandopril",paste("Trandolapril"),paste(meddata$TYPE))

save(meddata,file="PERMITmeddata28.rda")

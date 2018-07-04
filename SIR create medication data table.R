library("stringr")
library("lubridate")
#library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("tidyverse")
library("stringr")

setwd("~/Peek_PERMIT")

load("sir.data2yrsall.rda") #Full EHR extract
load("SIR_crea.repongoing_after_formula_variables.rda") #Primary table with 1 row per patient per date summarising the mean creatinine value per day, with pathology results
#inst<-read.csv("inst051217.csv") #Lookup regex file of unique textual precription instructions paired with implications for dose, number etc
inst <- read.csv("prescription_instructions.csv")
codes1<-read.csv("SIRdrugs.csv") %>%  #Lookup of medication type, family, active ingredient etc
          distinct(CODE, TYPE, DOSE_PER_TAB, .keep_all = TRUE) # keep only unique rows

#Subset extract if required to enable faster processing
sir.data <- sir.data %>%
  filter(event.date >= as.Date("2007-01-01") & event.date <= as.Date("2017-09-01")) %>%
    filter(ReadCode %in% codes1$CODE & PatientID %in% crea.rep$PatientID) %>%
      droplevels()
  
#Coerce date fields to date format if needed
sir.data$EntryDate<-as.Date(as.character(sir.data$EntryDate),format="%Y%m%d")


sub <- sir.data %>% 
          mutate(DESCRIPTION = gsub('[[:punct:]]','',CodeUnits),
                 DESCRIPTION = tolower(DESCRIPTION)) %>%
            select(PatientID,ReadCode,CodeValue, CodeUnits,DESCRIPTION, EntryDate) %>%
              distinct() # remove duplicates


#REPLACE WRITTEN NUMBERS WITH NUMBERS 1-6 IN PATIENT DATA (here already complete in instruction data lookup)

sub$DESCRIPTION<-gsub("one", "1", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("two", "2", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("three", "3", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("four", "4", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("five", "5", sub$DESCRIPTION) 
sub$DESCRIPTION<-gsub("six", "6", sub$DESCRIPTION) 

sub$DESCRIPTION<-tolower(sub$DESCRIPTION)#LOWER CASE
sub$DESCRIPTION<-gsub(" ", "", sub$DESCRIPTION, fixed = TRUE)  #REMOVE SPACES FROM INSTRUCTION STRINGS (This step already taken in instruction data lookup)
# inst<-inst %>% 
#         distinct(DESCRIPTION,.keep_all = TRUE)
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

ESS<- sub  %>% 
        distinct(CodeUnits, DESCRIPTION) %>% 
          anti_join(inst, by = c("CodeUnits", "DESCRIPTION"))

nrow(ESS)              
#[1] 0 all annotated

if(nrow(ESS) > 0){
  write.csv(ESS,file="EXTRADESCS.csv") #OUTPUT ANY NON PARSING DESCRIPTIONS. ADD TO THE REGEX TABLE, ANNOTATE AND RERUN
}

##################################################################################
#MARK PRESCRIPTIONS WHICH ARE INTENDED TO BE EXTRA TABLETS TO ADD TO AN EXISTING DOSE OF THE SAME DRUG.

EX<-sub$DESCRIPTION[grep("extra",sub$DESCRIPTION)]
AD<-sub$DESCRIPTION[grep("additionto",sub$DESCRIPTION)]
AD2<-sub$DESCRIPTION[grep("additional",sub$DESCRIPTION)]
sub$EXTRA2<-ifelse(sub$DESCRIPTION %in% AD | sub$DESCRIPTION %in% AD2 |sub$DESCRIPTION %in% EX,1,0)
#'EXTRA' MARKS PRESCRIPTIONS THAT ARE ADDITIONS OF MORE TO THE SAME DRUG, 
#OFTEN SUPPLEMENTING BOXED MEDICATION (E.G. 'TAKE AN EXTRA TABLET EVERY MORNING WITH THE ONE IN YOUR VENALINK').
############################################################################# 
#QUANTIFY MISSING PRESCRIPTION DATA

#sub<-merge(sub,inst,all.x=TRUE)
sub <- sub %>%
  left_join(inst, by = c("CodeUnits","DESCRIPTION"))

str(sub)
# 'data.frame':  1128427 obs. of  18 variables:
# $ PatientID      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ ReadCode       : Factor w/ 642 levels "14L..","66R5.",..: 279 279 279 279 279 279 279 279 279 279 ...
# $ CodeValue      : Factor w/ 156 levels "","0","0.5","000000000P",..: 22 22 34 34 34 34 34 61 61 69 ...
# $ CodeUnits      : chr  "Tablets" "Tablets" "tablet(s)" "tablet(s)" ...
# $ DESCRIPTION    : chr  "tablets" "tablets" "tablets" "tablets" ...
# $ EntryDate      : Date, format: "2007-03-26" "2007-04-30" "2007-08-02" "2007-09-18" ...
# $ EXTRA2         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ To.check       : Factor w/ 2 levels "","x": 1 1 1 1 1 1 1 1 1 1 ...
# $ EXTRA          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: Factor w/ 64 levels "0","0.07","0.1",..: 28 28 28 28 28 28 28 28 28 28 ...
# $ DAILY_DOSE     : Factor w/ 88 levels "","0.29","0.35",..: NA NA NA NA NA NA NA NA NA NA ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 43 levels "Amiodarone","Amoxicillin",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ DAYS           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ NUM2           : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...

#here with the last join we have the info that we need to spot equivalent records for which the only difference is how the prescription is spelled. e.g. take2onthefirstdaythen1eachday and take2todaythen1eachday can be considered as a duplicated record if prescribed for the same patient on the same day for the same drug

sub <- sub %>% 
  distinct(PatientID, ReadCode, CodeValue, EntryDate, EXTRA, .keep_all = TRUE)

length(sub$DESCRIPTION[sub$DESCRIPTION==""])
#[1] 4160

sub <- sub %>%
  rename(CODE = ReadCode)  #RENAME FIELDS IF NEEDED FOR LATER MERGING

#JOIN ON  DOSAGE DATA
subs <- sub %>%
  filter(CODE %in% codes1$CODE) %>%
    left_join(codes1, by = "CODE")

str(subs)
# 'data.frame':  1268192 obs. of  23 variables:
# $ PatientID      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ CODE           : chr  "META3900" "META3900" "META3900" "META3900" ...
# $ CodeValue      : Factor w/ 156 levels "","0","0.5","000000000P",..: 22 22 34 34 34 34 34 61 61 69 ...
# $ CodeUnits      : chr  "Tablets" "Tablets" "tablet(s)" "tablet(s)" ...
# $ DESCRIPTION    : chr  "tablets" "tablets" "tablets" "tablets" ...
# $ EntryDate      : Date, format: "2007-03-26" "2007-04-30" "2007-08-02" "2007-09-18" ...
# $ EXTRA2         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ To.check       : Factor w/ 2 levels "","x": 1 1 1 1 1 1 1 1 1 1 ...
# $ EXTRA          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: Factor w/ 64 levels "0","0.07","0.1",..: 28 28 28 28 28 28 28 28 28 28 ...
# $ DAILY_DOSE     : Factor w/ 88 levels "","0.29","0.35",..: NA NA NA NA NA NA NA NA NA NA ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 43 levels "Amiodarone","Amoxicillin",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ DAYS           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ NUM2           : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1164 levels "ACCUPRO 10mg-28CP tabs",..: 699 699 699 699 699 699 699 699 699 699 ...
# $ TYPE           : Factor w/ 108 levels "Aceclofenac",..: 65 65 65 65 65 65 65 65 65 65 ...
# $ DOSE_PER_TAB   : Factor w/ 58 levels "0.2","0.25","0.5",..: 23 23 23 23 23 23 23 23 23 23 ...
# $ FAMILY         : Factor w/ 10 levels "","ACEI","ALD_ANT",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...

# for some codes there are multiple rows in codes1 therefore nrow(subs)>nrow(sub). This is because some medications are composed of combinations of medications, and in the lookup file these are recorded in different rows.

########################################################################### 
#CALCULATE MISSING DATA WHERE POSSIBLE BASED ON PRESENT FIELDS
# convert to numeric relevant columns
subs$DAILY_DOSE<-as.numeric(as.character(subs$DAILY_DOSE))

subs$DOSE_PER_TAB<-as.numeric(as.character(subs$DOSE_PER_TAB))

subs$TABLETS_PER_DAY<-as.numeric(as.character(subs$TABLETS_PER_DAY))

subs$CodeValue <- as.numeric(as.character(subs$CodeValue))

summary(subs$CodeValue)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00     7.00    21.00    25.66    28.00 56120.00     7233
# the max values is unlikely let's see if there are others
subs %>% 
  group_by(CodeValue) %>% 
    count() %>% 
      arrange(desc(CodeValue))
# # A tibble: 143 x 2
# # Groups:   CodeValue [143]
# CodeValue     n
# <dbl> <int>
# 1     56120     2
# 2     28168     2
# 3      8428     1
# 4      2000     3
# 5      1000     3
# 6       900     1
# 7       843     2
# 8       815     1
# 9       600    63
# 10       596     3
# ... with 133 more rows

# remove rows with unlikely values
subs$CodeValue <- ifelse(subs$CodeValue > 365 | subs$CodeValue < 0.5,
                         NA,
                         subs$CodeValue)

summary(subs$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.3     2.1     6.0    31.7    15.0  4000.0 1254352

#most of daily_dose is NAs, let's try to use the data in the other columns to infer it

subs$DAILY_DOSE<-ifelse(is.na(subs$DAILY_DOSE)&
                          !is.na(subs$TABLETS_PER_DAY)&
                          !is.na(subs$DOSE_PER_TAB),
                        subs$TABLETS_PER_DAY*subs$DOSE_PER_TAB,
                        subs$DAILY_DOSE)

summary(subs$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    4.00   12.50   42.79   40.00 5000.00    7057 

#remove zeros
subs$DAILY_DOSE<-ifelse(subs$DAILY_DOSE == 0,
                        NA,
                        subs$DAILY_DOSE)

summary(subs$DAILY_DOSE)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.175    4.000   12.500   42.790   40.000 5000.000     7060 

sum(is.na(subs$DAILY_DOSE)/nrow(subs))
#[1] 0.00556698
# much better only a few are NAs now

summary(subs$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.000   1.000   1.219   1.000  21.000    3188

subs$TABLETS_PER_DAY<-ifelse(subs$TABLETS_PER_DAY == 0,
                             NA,
                             subs$TABLETS_PER_DAY)

summary(subs$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.070   1.000   1.000   1.219   1.000  21.000    3191

#only a minority is NA, let's try to infer it applying an inverse formula

subs$TABLETS_PER_DAY<-ifelse(is.na(subs$TABLETS_PER_DAY)&
                               !is.na(subs$DAILY_DOSE)&
                               !is.na(subs$DOSE_PER_TAB),
                             subs$DAILY_DOSE/subs$DOSE_PER_TAB,
                             subs$TABLETS_PER_DAY)

summary(subs$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.070   1.000   1.000   1.222   1.000  21.000       8 

summary(subs$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.0     1.0     1.0     4.5     1.0   180.0 1262884 

subs$DAYS <- ifelse(is.na(subs$DAYS) &
                      !is.na(subs$CodeValue) &
                      !is.na(subs$TABLETS_PER_DAY),
                    round(subs$CodeValue/subs$TABLETS_PER_DAY, 0),
                    subs$DAYS)

summary(subs$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    7.00    8.00   21.59   28.00 1120.00    7464

# remove zeros
subs$DAYS <- ifelse(subs$DAYS < 1,
                    NA,
                    subs$DAYS)

summary(subs$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.0     7.0     8.0    21.6    28.0  1120.0    7700

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

summary(subs$DAILY_DOSE)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.175    4.000   12.500   45.740   40.000 5000.000 

# do the same for number of days
b<-subs[,c("DAYS","TYPE")]
b<-na.omit(b)
b2<-b %>% 
  group_by(TYPE) %>% 
  summarise(MEDIAN_DAYS= median(DAYS, na.rm = TRUE)) %>% 
  as.data.frame

# append median_dose in the main dataset
subs <- subs %>%
  left_join(b2, by = "TYPE")

subs$DAYS<-ifelse(is.na(subs$DAYS), # substitute if missing
                        subs$MEDIAN_DAYS,
                        subs$DAYS)

summary(subs$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    7.00    8.00   21.52   28.00 1120.00 

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
str(subt)
# 'data.frame':  9004 obs. of  25 variables:
#   $ PatientID      : int  20 20 20 20 20 20 20 20 20 20 ...
# $ CODE           : chr  "e758." "e758." "e758." "e758." ...
# $ CodeValue      : num  8 8 8 8 8 8 8 8 8 8 ...
# $ CodeUnits      : chr  "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" ...
# $ DESCRIPTION    : chr  "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" ...
# $ EntryDate      : Date, format: "2011-03-21" "2011-03-21" "2011-05-27" "2011-05-27" ...
# $ EXTRA2         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ To.check       : Factor w/ 2 levels "","x": 1 1 1 1 1 1 1 1 1 1 ...
# $ EXTRA          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  2 2 2 2 2 2 2 2 2 2 ...
# $ DAILY_DOSE     : num  200 200 200 200 200 200 200 200 200 200 ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 43 levels "Amiodarone","Amoxicillin",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAYS           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ NUM2           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1164 levels "ACCUPRO 10mg-28CP tabs",..: 361 361 361 361 361 361 361 361 361 361 ...
# $ TYPE           : Factor w/ 108 levels "Aceclofenac",..: 34 34 34 34 34 34 34 34 34 34 ...
# $ DOSE_PER_TAB   : num  100 100 100 100 100 100 100 100 100 100 ...
# $ FAMILY         : Factor w/ 10 levels "","ACEI","ALD_ANT",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...
# $ MEDIAN_DOSE    : num  200 200 200 200 200 200 200 200 200 200 ...
# $ MEDIAN_DAYS    : num  1 1 1 1 1 1 1 1 1 1 ...

subnt<-subs[subs$THEN == 0,] # select single prescriptions
str(subnt)

# 'data.frame':  1263693 obs. of  25 variables:
# $ PatientID      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ CODE           : chr  "META3900" "META3900" "META3900" "META3900" ...
# $ CodeValue      : num  12 12 16 16 16 16 16 24 24 28 ...
# $ CodeUnits      : chr  "Tablets" "Tablets" "tablet(s)" "tablet(s)" ...
# $ DESCRIPTION    : chr  "tablets" "tablets" "tablets" "tablets" ...
# $ EntryDate      : Date, format: "2007-03-26" "2007-04-30" "2007-08-02" "2007-09-18" ...
# $ EXTRA2         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ To.check       : Factor w/ 2 levels "","x": 1 1 1 1 1 1 1 1 1 1 ...
# $ EXTRA          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAILY_DOSE     : num  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 43 levels "Amiodarone","Amoxicillin",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ DAYS           : num  12 12 16 16 16 16 16 24 24 28 ...
# $ NUM2           : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1164 levels "ACCUPRO 10mg-28CP tabs",..: 699 699 699 699 699 699 699 699 699 699 ...
# $ TYPE           : Factor w/ 108 levels "Aceclofenac",..: 65 65 65 65 65 65 65 65 65 65 ...
# $ DOSE_PER_TAB   : num  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 ...
# $ FAMILY         : Factor w/ 10 levels "","ACEI","ALD_ANT",..: 7 7 7 7 7 7 7 7 7 7 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...
# $ MEDIAN_DOSE    : num  2.14 2.14 2.14 2.14 2.14 2.14 2.14 2.14 2.14 2.14 ...
# $ MEDIAN_DAYS    : num  20 20 20 20 20 20 20 20 20 20 ...
############################################################################
subt$C1<-ifelse((duplicated(subt$PatientID)&
                   duplicated(subt$EntryDate)&
                   duplicated(subt$TYPE)), # check if duplicated type
                1,
                0) 
table(subt$C1)
#    0    1 
# 2632 6372 

subt$EntryDateb<- subt$EntryDate+subt$DAYS # adjust date to plausible prescription date for the second prescription

summary(subt$EntryDateb)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2007-01-03" "2011-07-23" "2013-08-19" "2013-04-15" "2015-04-08" "2016-10-06"

#EDIT THE DOSES FOR THE REPLICATE ROWS (FOLLOWING DOSE CHANGE)
subt$CodeValue <- ifelse(subt$C1==1 &
                           !is.na(subt$CodeValue) &
                           !is.na(subt$DAYS) &
                           !is.na(subt$TABLETS_PER_DAY), 
                         subt$CodeValue - subt$DAYS * subt$TABLETS_PER_DAY,
                         subt$CodeValue)

summary(subt$CodeValue)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -14.000   6.000   6.000   8.757   8.000 815.000  

sum(subt$CodeValue <= 0)
#[1] 46

#there are negative CodeValues, that means that for those records GPs did not record the number of tablets but something else (maybe dose?). For the negative prescription we will put NA for now and then fill with the median if needed.

subt$CodeValue <- ifelse(subt$CodeValue <= 0, 
                         NA,
                         subt$CodeValue)

subt$DAILY_DOSE<-ifelse(subt$C1==1, 
                        subt$DOSE2,
                        subt$DAILY_DOSE)

summary(subt$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.28  200.00  200.00  188.50  200.00 1500.00    6287

subt$TABLETS_PER_DAY<-ifelse(subt$C1==1, #change number of tablets per day
                        subt$NUM2,
                        subt$TABLETS_PER_DAY)

summary(subt$TABLETS_PER_DAY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.140   1.000   1.000   1.299   2.000   8.000      23 

subt$DAILY_DOSE <- ifelse(is.na(subt$DAILY_DOSE) &
                            !is.na(subt$TABLETS_PER_DAY) &
                            !is.na(subt$DOSE_PER_TAB),
                          subt$TABLETS_PER_DAY * #let's use the info on tab per day and dose per tab to fill the missing info
                            subt$DOSE_PER_TAB,
                          subt$DAILY_DOSE)

summary(subt$DAILY_DOSE)

#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.28  100.00  100.00  125.40  200.00 1500.00      12

# #let's use the median for the NAs 

subt$DAILY_DOSE <- ifelse(is.na(subt$DAILY_DOSE),
                          subt$MEDIAN_DOSE,
                          subt$DAILY_DOSE)

summary(subt$DAILY_DOSE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.28  100.00  100.00  125.30  200.00 1500.00

subt$DAYS <- ifelse(subt$C1==1 &
                      !is.na(subt$CodeValue) &
                      !is.na(subt$TABLETS_PER_DAY),
                    round(subt$CodeValue/subt$TABLETS_PER_DAY, 0),
                    subt$CodeValue)

summary(subt$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   6.000   6.000   8.616   8.000 700.000      48  

#let's use the median for the NAs
subt$DAYS <- ifelse(is.na(subt$DAYS),
                          subt$MEDIAN_DAYS,
                          subt$DAYS)

summary(subt$DAYS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   6.000   6.000   8.623   8.000 700.000 

# modify the entrydate for replicates
subt$EntryDate <- ifelse(subt$C1 == 1,
                         subt$EntryDateb,
                         subt$EntryDate)

summary(subt$EntryDate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13520   15180   15940   15810   16530   17080

subt$EntryDate <- subt$EntryDate + as.Date("1970-01-01")

summary(subt$EntryDate)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2007-01-02" "2011-07-22" "2013-08-18" "2013-04-15" "2015-04-08" "2016-10-06" 

subt<-subt %>% 
        select(-C1, -EntryDateb)

meddata<-rbind(subt,subnt)
str(meddata)

# 'data.frame':  1272697 obs. of  25 variables:
# $ PatientID      : int  20 20 20 20 20 20 20 20 20 20 ...
# $ CODE           : chr  "e758." "e758." "e758." "e758." ...
# $ CodeValue      : num  8 6 8 6 8 6 8 6 8 6 ...
# $ CodeUnits      : chr  "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" "TWO NOW THEN ONE DAILY" ...
# $ DESCRIPTION    : chr  "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" "2nowthen1daily" ...
# $ EntryDate      : Date, format: "2011-03-21" "2011-03-22" "2011-05-27" "2011-05-28" ...
# $ EXTRA2         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ To.check       : Factor w/ 2 levels "","x": 1 1 1 1 1 1 1 1 1 1 ...
# $ EXTRA          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TABLETS_PER_DAY: num  2 1 2 1 2 1 2 1 2 1 ...
# $ DAILY_DOSE     : num  200 100 200 100 200 100 200 100 200 100 ...
# $ ALT_OTHER_MEDS : Factor w/ 3 levels "1","Replace",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP            : Factor w/ 43 levels "Amiodarone","Amoxicillin",..: NA NA NA NA NA NA NA NA NA NA ...
# $ REP2           : Factor w/ 3 levels "Bumetanide","Furosemide",..: NA NA NA NA NA NA NA NA NA NA ...
# $ THEN           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DAYS           : num  8 6 8 6 8 6 8 6 8 6 ...
# $ NUM2           : num  1 1 1 1 1 1 1 1 1 1 ...
# $ DOSE2          : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DESC           : Factor w/ 1164 levels "ACCUPRO 10mg-28CP tabs",..: 361 361 361 361 361 361 361 361 361 361 ...
# $ TYPE           : Factor w/ 108 levels "Aceclofenac",..: 34 34 34 34 34 34 34 34 34 34 ...
# $ DOSE_PER_TAB   : num  100 100 100 100 100 100 100 100 100 100 ...
# $ FAMILY         : Factor w/ 10 levels "","ACEI","ALD_ANT",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ X              : Factor w/ 2 levels "","Loop Diuretics": 1 1 1 1 1 1 1 1 1 1 ...
# $ MEDIAN_DOSE    : num  200 200 200 200 200 200 200 200 200 200 ...
# $ MEDIAN_DAYS    : num  1 1 1 1 1 1 1 1 1 1 ...

############################################################################
#ASSIGN DATE OF END OF PRESCRIPTION
meddata$END_DATE<-ifelse(!is.na(meddata$DAYS),
                          meddata$EntryDate+as.difftime(meddata$DAYS, unit="days"),
                         NA)
summary(meddata$END_DATE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13520   14510   15290   15310   16120   17740

meddata$END_DATE<-as.Date(meddata$END_DATE,origin="1970-01-01")

summary(meddata$END_DATE)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2007-01-05" "2009-09-23" "2011-11-10" "2011-12-01" "2014-02-15" "2018-07-27"

save(meddata,file="SIR_PERMITmeddata28.rda")

############################################################################
#STOP/REPLACE INSTRUCTIONS- ROWS WITH INSTRUCTIONS WHICH RELATE TO THE REPLACEMENT OF DRUGS WITH
#A NEW DRUG OR NEW LEVEL OF DOSE HAVE DATA WHICH HAS BEEN ENTERED IN THE ALT_OTHER_MEDS COLUMN 
#(Short for alterations to other medications)

#load("SIR_PERMITmeddata28.rda")

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

meddata %>% 
  group_by(REP) %>%
    count() %>%
      arrange(desc(n)) %>%
        print(n = 50)

# A tibble: 43 x 2
# Groups:   REP [43]
# REP                          n
# <fct>                    <int>
#   1 <NA>                   1268699
# 2 Spironolactone             358
# 3 Indapamide                 309
# 4 Furosemide                 241
# 5 Valsartan                  211
# 6 Telmisartan                205
# 7 Candesartantan             155
# 8 Ramipril                   152
# 9 Bendroflumethiazide        109
# 10 Eprosartan                  84
# 11 Bumetanide                  55
# 12 Lisinopril                  44
# 13 LISINOPRIL, FUROSEMIDE      44
# 14 Irbesartan                  30
# 15 Naproxen                    19
# 16 Candesartan                 17
# 17 Aspirin                     15
# 18 Fosinopril                   7
# 19 Atorvastatin                 6
# 20 BENDROFLUMETHIAZIDE          5
# 21 Amiodarone                   4
# 22 Co-Amilofruse                4
# 23 Diclofenac                   4
# 24 Digoxin                      4
# 25 Felodipine                   3
# 26 Azithromycin                 2
# 27 Clarithromycin               2
# 28 Ibuprofen                    2
# 29 Nitrofurantoin               2
# 30 Prosartan                    2
# 31 Triamterzide                 2
# 32 Amoxicillin                  1
# 33 Bendoflumethiazide           1
# 34 Bisprolol                    1
# 35 Cefadroxil                   1
# 36 Doxazosin                    1
# 37 Enalapril                    1
# 38 Ibruprofen                   1
# 39 Methotrexate                 1
# 40 Metolazone                   1
# 41 Nifedipine                   1
# 42 Nitofurantoin                1
# 43 Quinine                      1

#Check there are no situations in which 2 or more different drugs are simultaneously being replaced
head(meddata$REP2[!(meddata$REP2 %in% meddata$REP)]) #ALL THOSE IN REP2 ARE ALSO IN REP
# factor(0)
# Levels: Bumetanide Furosemide Ramipril

#Make a small subset table for processing purposes of ongoing drug prescriptions which are being replaced
#before their end dates so we can edit these rows
#restrict to drugs of interest to the study
r<-paste(unique(meddata$TYPE))

# keep only REP/REP2 in type
meddata$REP<-ifelse((meddata$REP %in% r) &
                      !is.na(meddata$REP),
                    paste(meddata$REP),
                    NA)

meddata$REP2<-ifelse(meddata$REP2 %in% r& !is.na(meddata$REP2),
                     paste(meddata$REP2),
                     NA) 

meddata$ALT_OTHER_MEDS<-ifelse(is.na(meddata$REP),
                               NA,
                               paste(meddata$ALT_OTHER_MEDS))
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

columns=c("PatientID","EntryDate","TYPE") #This should be PatientID, EntryDate and TYPE

dots<-lapply(columns, as.symbol)
first <-stops %>% 
  group_by_(.dots=dots) %>%
    as.data.frame 

library("survival")

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

summary(first$NEWENDDATE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 13540   15720   16380   16080   16810   17080  981701 

first$NEWENDDATE<-as.Date(first$NEWENDDATE,origin="1970-01-01")

summary(first$NEWENDDATE)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2007-01-22" "2013-01-17" "2014-11-10" "2014-01-08" "2016-01-12" "2016-10-10"     "981701"

first<-first[!is.na(first$NEWENDDATE)&
               first$NEWENDDATE>first$EntryDate &
               first$NEWENDDATE<first$END_DATE,
             c("PatientID","EntryDate","TYPE","NEWENDDATE")]

#Be careful with neardate- we have told it to look for entries after the EntryDate but if it does not find one
#it will look for one before EntryDate. Here we confirm that the stop instruction fell between the entry and original end date

length(first$PatientID) #Count how many prescriptions are affected
#[1] 41

meddata <- meddata %>% 
            left_join(first, by = c("PatientID", "EntryDate", "TYPE"))

#Each affected entry now has and ENDDATE and NEWENDDATE. We take the earlier of the two and replace ENDDATE then proceed as
#with all other unaffected rows.

meddata$END_DATE<-ifelse(!is.na(meddata$NEWENDDATE)&
                           meddata$END_DATE>meddata$NEWENDDATE &
                           meddata$EntryDate<meddata$NEWENDDATE,
                         meddata$NEWENDDATE,
                         meddata$END_DATE)

summary(meddata$END_DATE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13520   14510   15290   15310   16110   17740

meddata$END_DATE<-as.Date(meddata$END_DATE,origin="1970-01-01")
#save(meddata,file="PERMITmeddata28.rda") #If you want to overwrite at this point

summary(meddata$END_DATE)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2007-01-05" "2009-09-22" "2011-11-09" "2011-11-30" "2014-02-13" "2018-07-27"

#IF YOU HAVE ENTRIES IN REP2 AT THIS POINT YOU NEED TO REPEAT THE PROCESS AGAIN FOR REP2 AS FOR REP. 
#AND SO ON FOR REP3 ETC IF YOU NEED THIS IN YOUR SPECIFIC CONTEXT.
####################################################################################
#DEAL WITH 'EXTRAS'- SOMETIMES A DOSE SUPPLEMENTS ANOTHER OF THE SAME DRUG
#e.g. "TAKE IN ADDITION TO THE TABLET IN YOUR DOSETTE" if a dose increase is being made temporarily
#SUCH ENTRIES ARE MARKED WITH A 1 IN THE "EXTRA" COLUMN- THIS IS BINARY AND DOES NOT REFLECT TABLET NUMBER

#Create subsets with and without
# ex<-meddata[meddata$EXTRA==1,]
# subx<-meddata[meddata$EXTRA==0,]
# ex<-ex[ex$TYPE %in% subx$TYPE &
#          ex$PatientID %in% subx$PatientID &
#          ex$EntryDate>=subx$EntryDate &
#          ex$END_DATE<subx$END_DATE,]
# # 
# # #Why are we not calling meddata here? Because if we do this then we amend the rows in ex also and these become our neardate first matches.
# # #A good alternative would be for ex to be defined as meddate minus rows listed in ex with the existing date restrictions.
# # 
# ex[!is.na(ex$TYPE),] #Count how many specific entries are impacted- has been very few previously
# 
# #THE BELOW CODE SECTION WAS NOT APPLIED SO HAS NOT BEEN CHECKED FOR FUNCTION AS ONLY 2 ENTRIES WERE PREVIOUSLY AFFECTED. 
# #DUE TO TIME RESTRICTIONS IT MADE SENSE TO AMEND THESE DIRECTLY RATHER THAN SYSTEMATICALLY
# #AND BECAUSE THIS DATA WAS TABULATED IN CPRD
# #SO CHECK THE BELOW CAREFULLY
# 
# head(subx)
# smalltab<-ex[,c("PatientID","TYPE","EntryDate","END_DATE","DAILY_DOSE")]
# columns=names(smalltab[c(1:3)])
# dots<-lapply(columns, as.symbol)
# firstU <-smalltab %>% 
#   group_by_(.dots=dots) %>%
#     as.data.frame
# 
# firstU$AddDate<-NA
# firstU$AddDose<-NA
# for (i in 1:unique(as.factor(firstU$TYPE))){
# 
#   exb<-ex[ex$TYPE==firstU$TYPE[i],]
#   indx<-neardate(firstU$PatientID, 
#                  subx$PatientID, 
#                  firstU$EntryDate,
#                  subx$EntryDate,
#                  best="after")
#   
#   firstU$AddDate<-(ifelse(firstU$TYPE==firstU$TYPE[i],
#                           subx[indx,"EntryDate"],
#                           firstU$AddDate))
# 
#   firstU$AddDose<-(ifelse(firstU$TYPE==firstU$TYPE[i],
#                           subx[indx,"DAILY_DOSE"],
#                           firstU$AddDose))
# 
# }
# 
# #for each entry detailing a change, near date match to a prior entry of the same prescription
# #Mark when the additional dose is added and how many mg per day are being added.
# 
# firstU$AddDate<-as.Date(firstU$AddDate,origin="1970-01-01")
# head(firstU[!is.na(firstU$AddDate),])
# firstU<-firstU[!is.na(firstU$AddDate)&firstU$AddDate>=firstU$EntryDate&firstU$AddDate<firstU$END_DATE,c("PatientID","EntryDate","END_DATE","TYPE","AddDate","AddDose")]
# #Again, ensure the addition happens between the prescription start and end date
# 
# length(firstU$PatientID)#Count how many prescriptions are affected
# names(firstU)

#I didn't get this far but what needs to happen here is that rows in firstU are replicated, with truncation of the first copy of each row with the AddDate replacing ENDDATE and retention of the original DAILY_DOSE
#In the second copy of each row, AddDate becomes EntryDate and the DAILY-DOSE has the AddDose added on.
#See rows 176-260- you can use this as a template.

#firstU is then merged onto subx (all rows not in ex), and this collectively replaces the prior version of meddata.

##################################################################### 
#In meddata the most common oral liquid dose prescribed is a 5ml dose
#this is assumed if the instruction says something like "TAKE ONCE A DAY"
#All CodeValue values for oral liquids are given as mg/5ml in the original meddata file 

# #CONVERT VOLUME DOSAGES FOR LIQUID MEDS TO GET mg/ml. 
# #OMIT WATER TO AVOID PICKING UP SOLID DOSE MEDS INSTRUCTED TO BE DISSOLVED IN WATER.
# meddata$CodeValue <- ifelse(grepl("ml",meddata$DESCRIPTION)&
#                               !grepl("water",meddata$DESCRIPTION),
#                             as.numeric(meddata$CodeValue)/5,
#                             as.numeric(meddata$CodeValue))

meddata<-unique(meddata)
meddata$DAILY_DOSE<-signif(meddata$DAILY_DOSE,digits=2)

#In the original file two drug types were assigned the wrong family, this corrects that.
#You may prefer to permenantly correct this in the original file.
meddata$FAMILY<-ifelse(meddata$TYPE=="Doxycycline",paste("Antimicrobial"),paste(meddata$FAMILY))
meddata$FAMILY<-ifelse(meddata$TYPE=="Chloretracycline",paste("Chlortetracycline"),paste(meddata$FAMILY))

#You may or may not need the lines below here depending on what format you are in at this point
#meddata$EntryDate<-as.Date(meddata$EntryDate,format="%d/%m/%Y")
#meddata$END_DATE<-as.Date(meddata$END_DATE,format="%d/%m/%Y")

#Corrects two analagous types based on typing errors, again you may prefer to adapt the original file to correct this
meddata$TYPE<-ifelse(meddata$TYPE=="Hydrochlorothiazide",paste("Hydrochlorthiazide"),paste(meddata$TYPE))
meddata$TYPE<-ifelse(meddata$TYPE=="Trandopril",paste("Trandolapril"),paste(meddata$TYPE))

#This gives you your output file ready to add on the drugs columns to your main table, 1 row per prescribed drug per patient per date
save(meddata,file="SIR_PERMITmeddata28.rda")

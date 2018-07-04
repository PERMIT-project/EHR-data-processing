library("zoo")
#library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("tidyverse")
library("zoo")
library("data.table")
library("survival")
library("lubridate")

setwd("~/Peek_PERMIT")

load(file = "SIR_crea.repongoing_afterconditions.rda")

source('~/Peek_PERMIT/Simple formula variables.R')

source('~/Peek_PERMIT/SIR_Add smoking data.R')

save(crea.rep, file = "SIR_crea.repongoing_tmp.rda")

#### CKD diagnoses
dataset_name <- "SIR" # set up dataset name

eGFR_variable = "MDRDeGFR" # set up eGFR variable

source('~/Peek_PERMIT/Diagnose_CKD_based_on_eGFR.R')

save(crea.rep, file = "SIR_crea.repongoing_tmp.rda")

eGFR_variable = "CKDEPIeGFR" # set up eGFR variable
  
source('~/Peek_PERMIT/Diagnose_CKD_based_on_eGFR.R')
  
save(crea.rep, file = "SIR_crea.repongoing_with_CKD_diagnosis.rda")

###### R renal function monitoring variables

source('~/Peek_PERMIT/worsening.renal.function.R')

source('~/Peek_PERMIT/WRF_x.R')

save(crea.rep, file = "SIR_crea.repongoing_with_WRF.rda")

source('~/Peek_PERMIT/rate.of.renal.decline_github.R')

save(crea.rep, file = "SIR_crea.repongoing_after_formula_variables.rda")

##### check AKI

source('~/Peek_PERMIT/Diagnose_AKI_national_algorithm.R')

save(crea.rep, file = "SIR_crea.repongoing_after_formula_variables.rda")

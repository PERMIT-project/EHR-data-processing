library("zoo")
library("plyr")#If also loading dplyr ensure plyr is loaded first
library("dplyr")
library("tidyverse")
library("zoo")
library("data.table")
library("survival")
library("lubridate")

load(file = "SIR_crea.repongoing_afterconditions.rda")

source('~/Peek_PERMIT/Simple formula variables.R')

source('~/Peek_PERMIT/SIR_Add smoking data.R')

save(crea.rep, file = "SIR_crea.repongoing_with_formula_variables.rda")




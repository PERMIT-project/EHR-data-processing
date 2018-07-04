##### pipeline to be ran for SIR
setwd("~/Peek_PERMIT")

### create cohort
source('~/Peek_PERMIT/SIR study subject selection.R')

### append pathology data
source('~/Peek_PERMIT/SIR pair recent numeric pathology data.R')

### append comorbidities and death data
source('~/Peek_PERMIT/SIR mark first occurence of conditions.R')

### append variables calculated from formulas (e.g. eGFR, smoking, and algorithms [e.g. CKD, AKI, wrf])
source('~/Peek_PERMIT/SIR_calculate formula variables.R')

### assemble medication table 
source('~/Peek_PERMIT/SIR create medication data table.R')

### append prescriptions for individual medications
source('~/Peek_PERMIT/SIR add individual drug data.R')

### append prescriptions for drug families
source('~/Peek_PERMIT/SIR add drug data by family.R')

### add data source data
source('~/Peek_PERMIT/SIR enable random split by practice.R')

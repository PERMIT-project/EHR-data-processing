# EHR-data-processing

The codes within this repository can be adapted to create wide format patient data tables based on raw data fields called from within EHR system extracts of varying formats. 

Many of the commands in files x and x allow the creation of columns based on lookup clinical code lists such as those provided here in the 'Clinical Codes' folder, and can be adapted to a range of different clinical conditions as required. Other columns rely on pasting a numeric value for a pathology metric, and rely on the functionality of the Survival package (Thernau et al. 2017) to match most recent recorded values to row entry dates. Please use the link below to explore the documentation relating to the neardate function.

https://cran.r-project.org/web/packages/survival/index.html

SIR prescription description mining has been enabled here by a preconstructed table of all observed text instructions associated in our SIR extract with our medications of interest (inst050118.rda), which has been simplified through the removal of punctuation and case, censored where required to maintain anonymity and manually annotated. CPRD prescription data has been drawn from CPRD-provided lookup tables.

We recommend that logic pipelines used to process text based prescribing data can be adapted for non research-ready datasets if required from the PERL based programme 'Research Events Medication' (Williams, 2016, https://github.com/rw251).


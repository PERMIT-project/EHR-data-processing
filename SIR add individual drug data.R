#library("plyr")
library("dplyr")
library("lubridate")
library("tidyverse")
library("purrr")

load("SIR_crea.repongoing_after_formula_variables.rda")
load("PERMITmeddata28.rda") #LOAD ANNOTATED DRUG TABLE WITH ALL DATED PRECRIPTION ENTRIES PER PATIENT

dataset <- NULL

#LOOP THROUGH DRUGS
for (z in (1:length(unique(meddata$TYPE))) ){
  
  # keep only data for zth type
  p<-meddata[meddata$TYPE==unique(meddata$TYPE)[z], 
             c("PatientID","TYPE","EntryDate","DAILY_DOSE","END_DATE", "EXTRA")]
  
  #get only ids and creatinine date
  smalltab<-crea.rep[crea.rep$PatientID %in% p$PatientID,
                     c("PatientID","event.date")]
  
  #smalltab is a list of all the creatinine tests
  #p is a list of all of the prescriptions of the zth type
  
  p<-p[order(p$EntryDate,decreasing = TRUE), ]
    
  N <- p %>%
        inner_join(smalltab, by = "PatientID") %>% # only patients with data for the zth medication
          filter(EntryDate < event.date & # only prescription started before the creat measurement 
                  END_DATE >= event.date) %>% # only prescription still in place at the creat measurement date
            #select(PatientID,TYPE,DAILY_DOSE,event.date) %>%
              arrange(PatientID, event.date, EntryDate) %>%
                group_by(PatientID,TYPE, event.date) %>% # for each patient, type, and creat measurement
                  summarise(DAILY_DOSE = ifelse(EXTRA[length(EXTRA)] == 1, # check if the last one is EXTRA 
                                                  ifelse(length(EXTRA) > 1,
                                                         sum(DAILY_DOSE[(length(EXTRA) - 1): length(EXTRA)]),
                                                         DAILY_DOSE[length(EXTRA)]), # if not we take the last one
                                                  DAILY_DOSE[length(EXTRA)]),
                              EXTRA = EXTRA[length(EXTRA)] == 1) # if yes we sum them
  
  
  # add N to dataset
  if(nrow(N) > 0){
    
    temp_dataset <- N %>% 
                       ungroup() %>% 
                        select(PatientID, event.date, DAILY_DOSE)
    
    temp_dataset$yn<-1
    
    #check if dataset is not null to add the zth result
    if(!is.null(dataset)){
      
      dataset<-dataset %>% 
        full_join(temp_dataset, by = c("PatientID", "event.date")) # full join to keep in patients without presc for z
      
    } else{
      
      dataset <- temp_dataset
      
    }
    
    
    # modify name for zth doses
    colnames(dataset)[colnames(dataset) == "DAILY_DOSE"] <- paste0(unique(meddata$TYPE)[z],
                                                          '_dos',sep="")
    # modify bool indicator name for zth
    colnames(dataset)[colnames(dataset) == "yn"] <- paste0(unique(meddata$TYPE)[z],
                                                  '_yn',
                                                  sep="")
    
    
    rm(temp_dataset)
    
  } 
 
}

crea.rep<- crea.rep %>% 
            left_join(dataset, by = c("PatientID", "event.date")) # add the results to main dataset


# fill NAs coming from the joints with zeros
cols_to_fill <- colnames(dataset)

cols_to_fill <- cols_to_fill[!(cols_to_fill %in% c("PatientID", "event.date"))] # remove first two columns

# for each column to fill
for(i in 1:length(cols_to_fill)){
  
  crea.rep[[cols_to_fill[i]]] <- replace_na(crea.rep[[cols_to_fill[i]]], 0)
  
}

save(crea.rep, file = "SIR_crea.repongoing_with_individual_drugs.rda")

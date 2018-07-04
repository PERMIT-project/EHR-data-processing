#ADDS CATEGORICAL DRUG DATA BY DRUG TYPE (E.G. NSAIDS) WHERE LARGE NUMBERS OF DRUG COMBINATIONS ARE CONSIDERED
#NB APPEND CLAUSES ARE USED IN TABLE FORMATION- IF ERROR MADE, DELETE OUTPUT TABLE BEFORE RERUNNING NEW ITERATIONS

library("stringr")
#library("plyr")
library("lubridate")
library("dplyr")
library("tidyverse")

load("SIR_crea.repongoing_with_individual_drugs.rda")
load("SIR_PERMITmeddata28.rda")

codes <- read.csv("SIRdrugs.csv")

meddata<-unique(meddata)

meddata$DAILY_DOSE<-signif(meddata$DAILY_DOSE,digits=2)

meddata$FAMILY<-ifelse(meddata$TYPE=="Doxycycline",paste("Antimicrobial"),paste(meddata$FAMILY))

meddata$FAMILY<-ifelse(meddata$TYPE=="Chloretracycline",paste("Chlortetracycline"),paste(meddata$FAMILY))
################################################################################################
# the following piece of code aims at identify each possible combination of drugs and doses fpr each grug family. All unique combinations are then categorised with a two digits system that assigns an integer to each possible drugs combination and an integer to each possible doses combination. These two numbers are separated by . (e.g. 1.2 with 1 referring to the drugs combination and 2 referring to the doses combination). Each category label is unique across the drugs family, but not across the all dataset (e.g. 1.2 means different things for one family and something else for a different one). To go back to the meaning of the category, a lookup table is saved as a csv file.

family_drugs <- codes %>%
  distinct(FAMILY, TYPE) %>% # get all unique medications per family
    arrange(FAMILY, TYPE) %>%
      filter(FAMILY != "")

families <- as.character(unique(family_drugs$FAMILY)) # get all families

family_prescription_categories <- NULL # initialise look up table

for(i in 1:length(families)){ # for each family

  family_drugs_i <- family_drugs %>%
    filter(FAMILY == families[i]) # get drugs for each family
  
  family_name_i <- str_replace_all(families[i], " ", "")
  
  prescription <- rep("", times = nrow(crea.rep))

  for(j in 1:nrow(family_drugs_i)){
    
    name <- paste(family_drugs_i$TYPE[j],"_dos", sep = "") # create colmun's name to extract
    
    vect <- crea.rep[[name]] # extract data
    
    if(!is.null(vect)){ # if the column exist
      prescription <- ifelse(vect != 0, # check if there is an active prescription
                             str_c(prescription, # append the presciption to previous active ones from the same family on the same row
                                   family_drugs_i$TYPE[j], 
                                   vect),
                             prescription)
    }
    
  }
  
  # remove spaces from string
  prescription <- str_replace_all(prescription, " ", "")
  
  #create df to merge later to crea.rep
  family_prescription <- data_frame(PatientID = crea.rep$PatientID,
                                    event.date = crea.rep$event.date,
                                    family_prescription = prescription,
                                    family = ifelse(prescription != "",
                                                    1,
                                                    0)) # TRUE if there is a presctiption
  
  # create family prescription categories
  prescription_unique  <- unique(prescription) %>%
                            sort()
  
  categories <- data_frame(prescription_unique = prescription_unique,
                           drugs = str_replace_all(prescription_unique, pattern = "[[:digit:]]+\\.*[[:digit:]]*", replacement = ","), #remove all numbers
                           doses = str_replace_all(prescription_unique, pattern = "[aA-zZ]+", replacement = ",")) %>% # remove all letters
    mutate(drugs = str_replace(drugs, pattern = "\\,$", replacement = ""), #remove commas at the end
           doses = str_replace(doses, pattern = "^\\,", replacement = "")) # remove commas at the beginning
  
  categories <- data_frame(drugs = unique(categories$drugs)) %>% # get all drugs combinations
    filter(drugs != "") %>%   
      mutate(drugs_cat = 1:length(drugs)) %>% # assign cat level
        right_join(categories, by = "drugs") # add to categories
  
  categories <- data_frame(doses = unique(categories$doses)) %>% # get all doses combinations
    filter(doses != "") %>%   
      mutate(doses_cat = 1:length(doses)) %>% # assign cat level
        right_join(categories, by = "doses") # add to categories
  
  categories <- categories %>%
    mutate(prescription_cat = as.numeric(paste(drugs_cat, doses_cat, sep = "."))) %>%  # create prescription cat
      filter(!is.na(doses_cat)) %>% #remove NA
        select(prescription_unique, drugs, drugs_cat, doses, doses_cat, prescription_cat) # reorder
        
  
  family_prescription_categories <- bind_rows(family_prescription_categories, # bind to other families drug
                                              categories  %>% 
                                                mutate(drugs_family = family_name_i))# assign family name to each row 
                                                  
  
  # we can now join the categories df to the family prescriptions
  family_prescription <- family_prescription %>%
    left_join(categories %>% 
                select(prescription_unique, prescription_cat), 
              by = c("family_prescription" = "prescription_unique")) %>%
      mutate(prescription_cat = as.factor(prescription_cat))
  
  # we have to change names before joining back to crea.rep so each family has a dedicated column
  colnames(family_prescription)[colnames(family_prescription) == "prescription_cat"] <- str_c(family_name_i, "cat", sep = "_")
  colnames(family_prescription)[colnames(family_prescription) == "family_prescription"] <- str_c(family_name_i, "desc") # change names
  colnames(family_prescription)[colnames(family_prescription) == "family"] <- str_c(as.character(family_name_i), "yn", sep = "_")
  
  
  crea.rep <- crea.rep %>% 
    left_join(family_prescription,
              by = c("PatientID", "event.date"))

}

# save lookup_table
family_prescription_categories %>%
  mutate(prescription_cat_sort = as.numeric(as.character(prescription_cat))) %>% #tmp column to better sort
    arrange(drugs_family, prescription_cat_sort) %>%
      select(-prescription_cat_sort) %>%
        write.csv("family_prescription_categories_lookup.csv", row.names = FALSE)


save(crea.rep, file = "SIR_crea.repongoing_with_drugs_by_family.rda")
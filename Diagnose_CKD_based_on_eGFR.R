library("dplyr")
library("doParallel")
#library("tidyverse")
library("foreach")
library("stackoverflow")
library("tidyr")

###########algorithm to check CKD status prospectively
check.CKD.threshold=function(d,ID,threshold){
  # get only current patient data
  d <- d %>% 
    filter(PatientID == ID)
  # initialise control, counter and CKD index
  control <- FALSE
  i <- 1
  index <- 0
  
  # for each eGFR, until condition something is found
  while((!control) &( i<nrow(d))){
    
    # check whether eGFR is below threshold
    if((d[i,]$CKDEPIeGFR<threshold)){
      
      # initialise partial counter
      j <- 1
      
      # until there is data or something is found
      while((!control)&(j<=(dim(d)[1]-i))){
        
        # check if next eGFR are still impaired
        if((d[i+j,]$CKDEPIeGFR < threshold)){
          
          #calculate difference between dates
          diff <- (d[i+j,]$event.date- d[i,]$event.date)
          
          # if more than 3 months -> condition met and diagnose CKD
          if(diff >= 90){
            
            control <- TRUE
            index <- d[i+j,]$row_index
            
          }
        }else{
          
          # if eGFR normal, break internal while and get back to the main one
          j <- dim(d)[1]-i
        }
        
        # if still impaired eGFR but not for three months still, keep looking
        j=j+1
      }
    }
    
    # move to next eGFR
    i=i+1
  }
  
  rm(d)
  
  return(index)
  
}

obtain_index_CKD_diagnosis <- function(file_name, threshold, chunks_number = 20, cores_number){
  
  source('/mnt/bmh01-rds/Peek_PERMIT/CKD_algorithm_function.R', echo=TRUE)
  
  # load data
#   if(grepl(x = file_name, pattern = ".rds", fixed = TRUE)){ # if rds
#     
#     crea.rep <- readRDS(file_name)
#     
#   }else{
#     
    load(file_name) 
    
  #}
  
  
  # getting IDs of relevant patients
  IDs <- crea.rep %>%
    filter(CKDEPIeGFR < threshold) %>%
      select(PatientID) %>%
        arrange(PatientID)
  
  IDs <- as.integer(unique(IDs$PatientID))
  
  chunks <- chunk2(IDs, chunks_number)
  
  indexes <- NULL
  
  for(i in 1:length(chunks)){
    
#     if(grepl(x = file_name, pattern = ".rds", fixed = TRUE)){ # if rds
#       
#       crea.rep <- readRDS(file_name)
#       
#     }else{
      
      load(file_name) 
      
    #}
    
    
    # create index to use in algorithm
    crea.rep <- crea.rep %>% 
      mutate(row_index = 1:nrow(crea.rep)) %>% # create index to be used in algorithm
        mutate(PatientID = as.integer(PatientID)) %>%
          arrange(PatientID, event.date) %>% 
            select(PatientID, CKDEPIeGFR, event.date, row_index) %>% # keep only necessary info
              filter(PatientID %in% chunks[[i]]) # retain only patients in this chunk
    
    cl <- makeCluster(cores_number)
    registerDoParallel(cl)
    
    system.time(indexes_i <- foreach(x = chunks[[i]], .combine=c("rbind"), .packages = "dplyr", .export = ls.str(parent.frame()), .verbose = TRUE) %dopar% {
      
      data_frame(PatientID = x,
                 index = check.CKD.threshold(d = crea.rep,
                                               ID = x,
                                               threshold = threshold))
      
    })
    
    stopCluster(cl)
    
    indexes <- indexes %>% 
      bind_rows(indexes_i)
    
    rm(crea.rep)
    
  }
  
  return(indexes)
  
}
###################################################################

dataset_name <- "SIR"
#dataset_name <- "CPRD"

cores_number <- 60

file_name <- ifelse(test = dataset_name == "SIR",
                    yes = "crea.repongoing_witheGFR.rda",
                    no = "crea.rep.rda")

chunks_number <- ifelse(test = dataset_name == "SIR",
                    yes = 2,
                    no = 20)

#########check CKD 2-5
indexes_CKD2 <- obtain_index_CKD_diagnosis(file_name = file_name,
                                            threshold = 90,
                                            chunks_number = chunks_number,
                                            cores_number = cores_number) %>%
  rename(index_2 = index)

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))


#########check CKD 3-5
indexes_CKD3a <- obtain_index_CKD_diagnosis(file_name = file_name,
                                           threshold = 60,
                                           chunks_number = chunks_number,
                                           cores_number = cores_number) %>%
                  rename(index_3a = index)

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))

#########check CKD 3b-5
indexes_CKD3b <- obtain_index_CKD_diagnosis(file_name = file_name,
                                            threshold = 45,
                                            chunks_number = chunks_number,
                                            cores_number = cores_number) %>%
  rename(index_3b = index)

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))

#########check CKD 4-5
indexes_CKD4 <- obtain_index_CKD_diagnosis(file_name = file_name,
                                           threshold = 30,
                                           chunks_number = chunks_number,
                                           cores_number = cores_number) %>%
  rename(index_4 = index)

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))

#########check CKD 5
indexes_CKD5 <- obtain_index_CKD_diagnosis(file_name = file_name,
                                           threshold = 15,
                                           chunks_number = chunks_number,
                                           cores_number = cores_number) %>%
  rename(index_5 = index)

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))

########### reload full dataset
# if(grepl(x = file_name, pattern = ".rds", fixed = TRUE)){ # if rds
#   
#   crea.rep <- readRDS(file_name)
#   
# }else{
  
  load(file_name) 
  
#}

crea.rep <- crea.rep %>%
  mutate(row_index = 1:nrow(crea.rep)) %>% # create index to merge data
    mutate(PatientID = as.integer(PatientID)) %>%
        arrange(PatientID, event.date) %>% 
          select(PatientID, CKDEPIeGFR, event.date, row_index) # keep only necessary info

########### merge indexes
indexes_CKD <- data_frame(PatientID = unique(crea.rep$PatientID)) %>%
  left_join(indexes_CKD2, by = "PatientID") %>%
  left_join(indexes_CKD3a, by = "PatientID") %>%
    left_join(indexes_CKD3b, by = "PatientID") %>%
      left_join(indexes_CKD4, by = "PatientID") %>%
        left_join(indexes_CKD5, by = "PatientID") %>%
          left_join(crea.rep %>% 
                      select(PatientID, event.date, row_index), by = c("PatientID" = "PatientID", "index_2" = "row_index")) %>% # get CKD3 date
          rename(date_CKD2 = event.date) %>%
            left_join(crea.rep %>% 
                        select(PatientID, event.date, row_index), by = c("PatientID" = "PatientID", "index_3a" = "row_index")) %>% # get CKD3 date
                          rename(date_CKD3a = event.date) %>%
                left_join(crea.rep %>% 
                            select(PatientID, event.date, row_index), by = c("PatientID" = "PatientID", "index_3b" = "row_index")) %>% # get CKD3 date
                              rename(date_CKD3b = event.date) %>%
                      left_join(crea.rep %>% 
                          select(PatientID, event.date, row_index), by = c("PatientID" = "PatientID", "index_4" = "row_index")) %>% # get CKD4 date
                          rename(date_CKD4 = event.date) %>%
                            left_join(crea.rep %>% 
                              select(PatientID, event.date, row_index), by = c("PatientID" = "PatientID", "index_5" = "row_index")) %>% # get CKD5 date
                                rename(date_CKD5 = event.date) %>%
                                arrange(PatientID)  %>% 
                                mutate(CKD2 = !is.na(index_2) & (index_2 > 0),
                                       CKD3a = !is.na(index_3a) & (index_3a > 0),
                                       CKD3b = !is.na(index_3b) & (index_3b > 0),
                                       CKD4 = !is.na(index_4) & (index_4 > 0),
                                       CKD5 = !is.na(index_5) & (index_5 > 0))

save.image(paste("/mnt/bmh01-rds/Peek_PERMIT/",dataset_name,"_CKD_diag_tmp.RData", sep = ""))

# check CKD stages at the end of the study
prevalence <- c(CKD2 = sum(indexes_CKD$CKD2)/nrow(indexes_CKD),
                CKD3a = sum(indexes_CKD$CKD3a)/nrow(indexes_CKD),
                CKD3b = sum(indexes_CKD$CKD3b)/nrow(indexes_CKD),
                CKD4 = sum(indexes_CKD$CKD4)/nrow(indexes_CKD),
                CKD5 = sum(indexes_CKD$CKD5)/nrow(indexes_CKD))

prevalence

CKD_stages <- indexes_CKD %>%
  select(PatientID, date_CKD2:date_CKD5) %>%
    gather("stage", "date", -PatientID)  %>%
      filter(!is.na(date)) %>%
        arrange(PatientID, stage) %>%
          group_by(PatientID) %>%
            filter(stage == max(stage)) %>%
              ungroup() %>%
                group_by(stage) %>%
                  count()

CKD_stages %>%
  mutate(perc = n/nrow(indexes_CKD))

########## save before mearging
saveRDS(indexes_CKD, paste(dataset_name,"_CKD_index.rds", sep = ""))


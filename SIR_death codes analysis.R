library("stringr")

tmp <- first %>%
  mutate(DeathDate = as.Date(as.character(DeathDate),format="%Y%m%d")) %>%
    left_join(sir.data %>% 
                select(-DeathDate), by = "PatientID") %>%
      filter(event.date > DeathDate) %>%
        select(PatientID, DeathDate, event.date, ReadCode, Rubric) %>%
          mutate(diff = as.numeric(event.date - DeathDate))

length(unique(tmp$PatientID))

#[1] 1044

                      
##### create data summaries 
tmp_drugs <- tmp %>% 
              filter(grepl(pattern = "^[a-z]", x = ReadCode) | # select only prescriptions
                       grepl(pattern = "^[A-Y]{2,}", x = ReadCode) & str_length(as.character(ReadCode)) > 5) #emis prescriptions

tmp_exam <- tmp %>% 
            filter(grepl(pattern = "^[1-8]", x = ReadCode)) # select only examinations

tmp_admin <- tmp %>% 
              filter(grepl(pattern = "^9", x = ReadCode)) # select only admin codes

tmp_diag <- tmp %>%
              filter(grepl(pattern = "^[A-Z]", x = ReadCode)  & str_length(as.character(ReadCode)) <= 5) # select only diagnosis

tmp_other <- tmp %>%
              filter(!(ReadCode %in% tmp_drugs$ReadCode),
                     !(ReadCode %in% tmp_exam$ReadCode),
                     !(ReadCode %in% tmp_admin$ReadCode),
                     !(ReadCode %in% tmp_diag$ReadCode))

summarise_records <- function(d, db_name){
  
  d_summary <- d %>%
                group_by(PatientID) %>%
                  summarise(n = n(),
                            max_diff = max(diff),
                            unique_codes = length(unique(ReadCode)),
                            unique_dates = length(unique(event.date)))  
              
  colnames(d_summary)[colnames(d_summary) == "n"] <- str_c(db_name, "_", "n")            
  colnames(d_summary)[colnames(d_summary) == "max_diff"] <- str_c(db_name,  "_","max_diff" )
  colnames(d_summary)[colnames(d_summary) == "unique_codes"] <- str_c(db_name,  "_","unique_codes" )
  colnames(d_summary)[colnames(d_summary) == "unique_dates"] <- str_c(db_name,  "_","unique_dates" )
  
  d_summary
}

overall_summary <- summarise_records(tmp_drugs, "drugs") %>%
                    full_join(summarise_records(tmp_exam, "exams"), by = "PatientID") %>%
                      full_join(summarise_records(tmp_admin, "admin"), by = "PatientID") %>%
                        full_join(summarise_records(tmp_diag, "diag"), by = "PatientID") %>%
                          full_join(summarise_records(tmp_other, "other"), by = "PatientID") %>%
                            full_join(summarise_records(tmp, "overall"), by = "PatientID") %>%
                              mutate(only_drugs_data = !is.na(drugs_n) &
                                                              is.na(exams_n) &
                                                                is.na(admin_n) &
                                                                  is.na(diag_n) &
                                                                    is.na(other_n),
                                     only_exams_data = is.na(drugs_n) &
                                                        !is.na(exams_n) &
                                                          is.na(admin_n) &
                                                            is.na(diag_n) &
                                                              is.na(other_n),
                                     only_admin_data = is.na(drugs_n) &
                                                        is.na(exams_n) &
                                                          !is.na(admin_n) &
                                                            is.na(diag_n) &
                                                              is.na(other_n),
                                     only_diag_data = is.na(drugs_n) &
                                                          is.na(exams_n) &
                                                            is.na(admin_n) &
                                                              !is.na(diag_n) &
                                                                is.na(other_n),
                                     only_other_data = is.na(drugs_n) &
                                                          is.na(exams_n) &
                                                            is.na(admin_n) &
                                                              is.na(diag_n) &
                                                                !is.na(other_n),
                                     dead = ifelse(overall_max_diff <= 90 & only_drugs_data |
                                                   overall_max_diff <= 30,
                                                   "TRUE",
                                                   ""))

table(overall_summary$dead)

#     TRUE 
# 120  924 

#120 patients for whom to go through summary records

write.csv(overall_summary, "overall_summary.csv", row.names = FALSE)


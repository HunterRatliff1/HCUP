library(tidyverse)
library(icd)    # helps with ICD codes 
# devtools::install_github("jackwasey/icd")
library(touch)  # Tools Of Unilization and Cost in Healthcare
library(googlesheets4)


########################################
###   Read data from google sheets   ###
### -------------------------------- ###
ss <- gs4_get("https://docs.google.com/spreadsheets/d/166lGOoZZFeuD4eBXyZQVkWyYw0XnkwhbAsQEEEQ-I7c/edit#gid=0")


# List of all tabs in sheet
ss_tab_names <- sheet_names(ss) %>%
  str_subset("\\*", negate = T)   # ignore anything with asterisk


# Get the data for all these sheets
ss_data <- ss_tab_names %>%
  map(~read_sheet(ss=ss, sheet=.x))  %>%
  set_names(ss_tab_names)


#######################################
###   Make custom comorbidity map   ###
### ------------------------------- ###
my_map <- ss_data %>%
  
  # Only sheets with Comorbidity as a column
  keep(~"Comorbidity" %in% names(.x)) %>%
  
  map(~.x$ICD10) %>%
  as.comorbidity_map()

print(my_map)  

# Use sample dataset from icd package
icd10_comorbid(uranium_pathology, 
  map = my_map)

icd10_comorbid(uranium_pathology, 
  map = my_map) %>%
  
  plot_comorbid_results()


#########################################
###   Make custom map for procedures  ###
### --------------------------------- ###
procedure_map <- ss_data %>%
  keep(~"I10_PR" %in% names(.x)) %>%
  
  map(~.x$I10_PR) %>%
  as.comorbidity_map()
  







#######################
###   Test it out   ###
### --------------- ###
df_dx <- read_sheet(ss, "TestingData*") %>%
  select(-starts_with("PR")) %>%
  pivot_longer(cols = starts_with("DX"), 
               names_to = "dxNum", values_to = "ICD10_DX") %>%
  filter(!is.na(ICD10_DX))


df_pr <- read_sheet(ss, "TestingData*") %>%
  select(-starts_with("DX")) %>%
  pivot_longer(cols = starts_with("PR"), 
               names_to = "prNum", values_to = "ICD10_PR") %>%
  filter(!is.na(ICD10_PR))


df_dx %>% 
  icd10_comorbid(visit_name = "Visit_ID", icd_name="ICD10_DX",
                 map = my_map)

df_pr %>% 
  icd10_comorbid(visit_name = "Visit_ID", icd_name="ICD10_PR",
                 map = procedure_map)
  
rm(df_pr, df_dx)

#########################
###   Use 2017 NTDB   ###
### ----------------- ###
library(dbplyr)
library(sqldf)
db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/Github/NTDB/ntdb.db")


# dbListTables(db) %>% str_subset("2017") 
# tbl(db, "2017_ICDDIAGNOSIS")


ntdb_procedures <- tbl(db, "2017_ICDPROCEDURE") %>%
  filter(ICDPROCEDURECODE %in% !!unlist(procedure_map)) %>%
  collect()

ntdb_procedures %>% 
  icd10_comorbid(visit_name = "inc_key", icd_name="ICDPROCEDURECODE",
                 map = procedure_map, return_df = TRUE) %>%
  
  as_tibble() 
  

ntdb_dx <- tbl(db, "2017_ICDDIAGNOSIS") %>%
  semi_join({
    tbl(db, "2017_ICDPROCEDURE") %>% filter(ICDPROCEDURECODE %in% !!unlist(procedure_map))
  }) %>%
  collect()

ntdb_dx %>% 
  icd10_comorbid(visit_name = "inc_key", icd_name="ICDDIAGNOSISCODE",
                 map = my_map, return_df = F) %>%
  
  as_tibble() %>%
  plot_comorbid_results()
  
rm(ntdb_dx, ntdb_procedures)  
  
# Close connection
dbDisconnect(db)
rm(db)







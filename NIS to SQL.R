library(tidyverse)
library(dbplyr)
library(sqldf)

csv_folder <- rstudioapi::selectDirectory(caption = "Select the root of the flash drive")

#######################################
###   Open connection to database   ###
### ------------------------------- ###
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     # user     = "hunter",
                     # password = rstudioapi::askForPassword("Database password"),
                     dbname = "nis.db")

dbListTables(db) # list of tables in db


#############################################
###   Construct list of available files   ###
### ------------------------------------- ###
files_df <- tibble(
  table_name = list.files(path=csv_folder, pattern = ".csv"),
  file_path  = list.files(path=csv_folder, pattern = ".csv", full.names = T)
) %>%
  mutate(size_MB    = file.size(file_path) / 1e6, # size of file in MB
         table_name = str_remove(table_name, "NIS_"),
         table_name = str_remove(table_name, ".csv")) %>%
  
  # Drop any files that are already in the database
  filter(!table_name %in% dbListTables(db))



#############################
###   Write to database   ###
### --------------------- ###
# If problems occur with fread, can also try vroom
walk2(files_df$table_name, files_df$file_path,
      ~dbWriteTable(conn = db, name = .x, value = data.table::fread(file=.y),
                    row.names = F, header = T, overwrite=F))


###########################################
###   Define function to index tables   ###
### ----------------------------------- ###
make_indices <- function(index_name, database, subset) {
  ### index_name:  String indicating column to index
  ### database:    Connection to SQLite database
  ### subset:      If provided, a regular expression used to limit the tables to index
  ###              (useful if you've already indexed some years and are adding to the database)
  
  
  ### First get a list of all the tables that have the index name as a column
  tables_to_index <- dbListTables(database) %>%
    
    # For each table, list the fields
    map(~dbListFields(database, .x)) %>%
    map(str_to_upper) %>% # make them all upper-case
    
    # Make the list named by the table name
    set_names( dbListTables(database) ) %>%
    
    
    # filter to only include tables with a column `index_name`
    keep(~index_name %in% .x) %>%
    names() # get the names of those tables
  
  ### If subset argument provided, limit to tables that match
  if(!missing(subset)) tables_to_index <- tables_to_index %>% str_subset(subset)
  
  
  
  
  ### Create index for these tables (silently)
  tables_to_index %>%
    walk(  ~db_create_index(database, table=.x, columns=index_name)  )

  ### Analyze them
  tables_to_index %>%
    walk(  ~db_analyze(database, table=.x)  )
  
  
  
  ### Report what was indexed (give pretty output if crayon installed)
  if("crayon" %in% rownames(installed.packages())) {
    message(crayon::silver("Indexed table(s)", str_c(crayon::red(tables_to_index), collapse = ", "), 
            "with key", crayon::bold$green(index_name)))
  } else(
    message("Indexed table(s) ", str_c(tables_to_index, collapse = ", "), 
            " with key `", index_name, "`")
  )
  
  
}



##################################
###   Tidy diagnosis table     ###
### -------------------------- ###
# This code may not work for future years, but this attempts
# to make a tidy diagnosis table, by melting/gathering the 
# columns DX(n) and DXCCS(n) for each patient

## WARNING: Loads nearly 7,300,000 records (3 GB) into RAM
dx_table <- tbl(db, "2012_Core") %>%
  select(KEY_NIS, HOSP_NIS, YEAR, NDX, starts_with("DX")) %>%
  collect()

dx_table <- dx_table %>%
  mutate_at(vars(starts_with("DX")), as.character) %>%
  
  # melt/gather
  pivot_longer(
    starts_with("DX"),
    names_to = c(".value", "diagnosis_number"), 
    names_pattern = "([:alpha:]*)(\\d*)"
  ) %>% 
  
  # clean up variable formats
  mutate(DX    = na_if(DX, ""),
         DXCCS = na_if(DXCCS, ""),
         diagnosis_number = as.numeric(diagnosis_number)) %>%
  
  # You can verify that this is correct, because the 
  # max(diagnosis_number) <= NDX
  filter(!is.na(DX)) 


### Write to database
dbWriteTable(conn = db, name = "2012_DX", value = dx_table,
             row.names = F, header = T, overwrite=F)
rm(dx_table)

### Index table
db_create_index(db, table="2012_DX", columns="KEY_NIS")  
db_create_index(db, table="2012_DX", columns="HOSP_NIS")  
db_create_index(db, table="2012_DX", columns="YEAR")  

### Analyze them
db_analyze(db, table="2012_DX")




##################################
###   Tidy procedure table     ###
### -------------------------- ###
# Same as above, but with procedures

## WARNING: Loads nearly 7,300,000 records (3 GB) into RAM
##          Takes ~90 seconds
PR_table <- tbl(db, "2012_Core") %>%
  select(KEY_NIS, HOSP_NIS, YEAR, NPR, starts_with("PR")) %>%
  collect()

  
PR_table <- PR_table %>%
  
  # melt/gather
  pivot_longer(
    starts_with("PR"),
    names_to = c(".value", "procedure_number"), 
    names_pattern = "([:alpha:]*)(\\d*)"
  ) %>%
  
  # clean up variable formats
  mutate(PR    = na_if(PR, ""),
         PRCCS = na_if(PRCCS, ""),
         PRDAY = na_if(PRDAY, ""),
         procedure_number = as.numeric(procedure_number)) %>%
  filter(!is.na(PR))


### Write to database
dbWriteTable(conn = db, name = "2012_PR", value = PR_table,
             row.names = F, header = T, overwrite=F)
rm(PR_table)

### Index table
db_create_index(db, table="2012_PR", columns="KEY_NIS")  
db_create_index(db, table="2012_PR", columns="HOSP_NIS")  
db_create_index(db, table="2012_PR", columns="YEAR")  

### Analyze them
db_analyze(db, table="2012_PR")



#############################
###   Index the tables    ###
### --------------------- ###
make_indices("KEY_NIS", db)
make_indices("HOSP_NIS", db)
make_indices("YEAR", db)


db_list_tables(db)
tbl(db, "sqlite_stat1")
rm(make_indices)




############################
###   Other HCUP tools   ###
### -------------------- ###
# This table contains 3 columns and can be used to explore the
# ICD-9 procedures (which aren't in the icd package). Source is below
# > https://www.hcup-us.ahrq.gov/toolssoftware/procedure/procedure.jsp
# 
# This isn't a very large file, but I'm writing it to the database since
# it might be helpful to use it before collecting data from SQL
pc2015 <- read_csv("HCUP Files/pc2015.csv", skip = 3,
                   col_names = c("PR", "PR_desc", "PR_cat")) %>%
  mutate(PR = str_remove_all(PR, "'"))

dbWriteTable(conn = db, name = "ICD9_PR", value = pc2015,
             row.names = F, header = T, overwrite=F)


# The Clinical Classifications Software (CCS) for ICD 10 helps
# to classify procedures into a schema
ccs_pr_icd10 <- read_csv("HCUP Files/ccs_pr_icd10pcs_2020_1.csv")




############################
###      Other code      ###
### -------------------- ###
tbl(db, "2012_Severity")   # HOSP_NIS, KEY_NIS
tbl(db, "2012_Hospital")   # HOSP_NIS, YEAR
tbl(db, "2012_Core")       # HOSP_NIS, KEY_NIS
tbl(db, "2012_DX_PR_GRPS") # HOSP_NIS, KEY_NIS


tbl(db, "2012_DX") %>% glimpse()


#-------------------------------#
####   Close db connection   ####
#-------------------------------#
dbDisconnect(db)  
rm(db)





# Used for survey design
library(survey)
library(srvyr) # has dplyr-syntax
options("survey.lonely.psu"="adjust")

# For database connections
library(dbplyr)
library(sqldf)
# See link for connecting to databases with survey package. Also see sqlsurvey
# > http://r-survey.r-forge.r-project.org/survey/svy-dbi.html

# Helpful example of survey data from HCUP
# > http://www.injuryepi.org/resources/Misc/hcupNotesMonet.pdf

library(tidyverse)
library(icd) # helps with ICD codes
library(touch)

#################################################################
###   Define some ICD codes to look at diabetic foot ulcers   ###
###   similar to DOI:10.1371/journal.pone.0134914             ###
### --------------------------------------------------------- ###
## They defined diabetic foot ulcers as having both:
## Ulcer codes: 707.1x or 707.9
## Diabetes 250.xx
ulcer_codes <- c(children("707.1"), "707.9") %>% decimal_to_short()
diabetes_codes <- children("250.")  %>% decimal_to_short()
# explain_code(ulcer_codes); explain_code(diabetes_codes); 


db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "nis.db")
dbListTables(db) # list of tables in db
# dbDisconnect(db) # close connection
# rm(db)


tbl(db, "2012_Core") %>% glimpse()

## Make table of all ulcer diagnoses (note: Pt's can have
## multiple codes, so may show up more than once)
ulcer_tbl <- tbl(db, "2012_DX") %>% 
  filter(DX %in% ulcer_codes) %>%
  select(KEY_NIS:YEAR, ulcer_DX_code=DX, ulcer_DX_Num=diagnosis_number) 
  
## Make table with diabetes diagnoses
DM_tbl <- tbl(db, "2012_DX") %>% 
  filter(DX %in% diabetes_codes) %>%
  select(KEY_NIS:YEAR, DM_DX_code=DX, DM_DX_Num=diagnosis_number) 


## This makes a table of our included patients
pt_data <- ulcer_tbl %>%                  # 101,528 obs
  semi_join(DM_tbl) %>%                   # 66,601 obs
  semi_join(tbl(db, "2012_Core"), .) %>%  # 62,290 obs
  
  # Drop diagnosis & procedure columns
  select(-starts_with("DX"), -starts_with("PR")) %>%
  collect() 



## Make surgical codes table, listing the procedure codes,
## description, and if it's a minor (84.10-84.12)
## or major amputation (84.13-84.19)
SxCode_tbl <- tbl(db, "ICD9_PR") %>%
  collect() %>% 
  filter(str_detect(PR, "^841")) %>%
  mutate(minor_amp = str_detect(PR, "^841[0-2]"))


## Makes table with everyone who had procedures
PR <- tbl(db, "2012_PR") %>%
  
  # Limit to those who had at least one of the procedures
  filter(PR %in% !!SxCode_tbl$PR) %>%
  collect() %>%
  
  # Limit to those who meet inclusion criteria
  semi_join(pt_data) %>%
  
  # Annotate procedures with names & major/minor flag
  inner_join(SxCode_tbl) %>%
  
  # Flag those with >1 amputations
  group_by(KEY_NIS) %>%
  mutate(multi_amp = n()>1) %>%
  ungroup()



ulcer_tbl %>%                  # 101,528 obs
  semi_join(DM_tbl) %>%
  count(KEY_NIS, sort=T)

  tbl(db, "ICD9_PR")

explain_table("N18.4")

pt_data %>%
  mutate(gotAmputation = KEY_NIS %in% PR$KEY_NIS)



dstrat <- svydesign(
  id = ~HOSP_NIS, 
  weights = ~DISCWT ,
  strata =  ~NIS_STRATUM,
  data = pt_data
)
summary(dstrat)

svymean(~NPR, dstrat) 
svytotal(~PAY1, dstrat)
count(pt_data, PAY1)

dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

svymean(~factor(stype),dclus1)


data.frame(
  visit_name = c("a", "b", "a"),
  icd9 = c("441", "412.93", "042")
) %>%
  icd9_comorbid_quan_deyo()


# tbl(db, "2012_Hospital") %>% count()
svydes<- svydesign(
  id = ~HOSPID , # Pretty sure this should be HOSP_NIS
  weights = ~DISCWT ,
  strata =  ~NIS_STRATUM,
  # strata = ~interaction(NIS_STRATUM , YEAR), # if multiple years are used
  # nest = TRUE,                               # specify these
  data = inj.long
)





svydesign
sqlsurvey(id = "key_ed", strata = "neds_stratum", weights = "discwt",
          key = "id", check.factors = fax, database = "~/monetInjury", driver = MonetDBLite(),
          table.name = "nedsinj0612"))



tbl(db, "2012_Core") %>% 
  filter(DRG==949) %>%
  filter(DRG!=DRG_NoPoA)
  glimpse()

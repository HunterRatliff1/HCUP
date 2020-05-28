library(tidyverse)
library(icd)    # helps with ICD codes 
# devtools::install_github("jackwasey/icd")
library(touch)  # Tools Of Unilization and Cost in Healthcare
library(googlesheets4); gs4_auth(email="hunterratliff1@gmail.com")

# Load helper functions
source("~/Github/HCUP/helper_functions.R") 
# See https://raw.githubusercontent.com/HunterRatliff1/HCUP/master/helper_functions.R


###############################
###        Read files       ###
### ----------------------- ###
gs4_auth(email="hunterratliff1@gmail.com")
ss <- gs4_get("https://docs.google.com/spreadsheets/d/166lGOoZZFeuD4eBXyZQVkWyYw0XnkwhbAsQEEEQ-I7c/edit#gid=0")
pr_ccs <- read_csv("HCUP Files/clean_ccs/ccs_pr_icd10.csv", col_types = cols(.default = col_character()))
dx_ccsR <- read_csv("HCUP Files/clean_ccs/ccsR_dx_icd10.csv", col_types = cols(.default = col_character()))



###############################
###           LEAs          ###
### ----------------------- ###
pr_ccs %>%
  filter(CCS_category=="157") %>% # Amputation of lower extremity
  select(I10_PR:I10_PR_Desc) %>%
  
  mutate(Laterality = case_when(
    str_detect(I10_PR_Desc, "Left")        ~ "Left",
    str_detect(I10_PR_Desc, "Right")       ~ "Right"),
    Level      = case_when(
      str_detect(I10_PR_Desc, "Toe")            ~ "Toe",
      str_detect(I10_PR_Desc, "Foot, Partial")  ~ "TMA",
      str_detect(I10_PR_Desc, "Foot, Complete") ~ "Foot",
      str_detect(I10_PR_Desc, "Lower Leg")      ~ "BKA",
      str_detect(I10_PR_Desc, "Knee")           ~ "Knee..?",
      str_detect(I10_PR_Desc, "Upper Leg")      ~ "AKA",
      str_detect(I10_PR_Desc, "Femoral")        ~ "AKA",
      str_detect(I10_PR_Desc, "Hindquarter")    ~ "Hindquarter"),
    Digit_num = str_extract(I10_PR_Desc, "\\d")
  ) %>%
  sheet_write(ss=ss, sheet="LEAs")



###############################
###           I&Ds          ###
### ----------------------- ###
pr_ccs %>%
  filter(CCS_category=="168") %>% # I&D; skin SQ tissue and fascia
  select(I10_PR:I10_PR_Desc) %>% 
  
  filter(str_detect(I10_PR_Desc, "Drain")) %>%
  filter(!str_detect(I10_PR_Desc, "Face|Back|Chest|Arm|Abd|Hand|Neck|Scalp|Head|Buttock")) %>%
  
  mutate(Area = case_when(
                  str_detect(I10_PR_Desc, "Foot")        ~ "Foot",
                  str_detect(I10_PR_Desc, "Up Leg")      ~ "Upper Leg",
                  str_detect(I10_PR_Desc, "Upper Leg")   ~ "Upper Leg",
                  str_detect(I10_PR_Desc, "Low Leg")     ~ "Lower Leg",
                  str_detect(I10_PR_Desc, "Lower Leg")   ~ "Lower Leg",
                  
                  
                  str_detect(I10_PR_Desc, "Inguinal")  ~ "Inguinal",
                  str_detect(I10_PR_Desc, "Femoral")   ~ "Femoral",
                  str_detect(I10_PR_Desc, "Pelvic")    ~ "Pelvic",
                  str_detect(I10_PR_Desc, "Perineum")  ~ "Perineum"),
  Laterality = case_when(
                 str_detect(I10_PR_Desc, "Left")        ~ "Left",
                 str_detect(I10_PR_Desc, "Right")       ~ "Right",
                 str_detect(I10_PR_Desc, " L ")         ~ "Left",
                 str_detect(I10_PR_Desc, " R ")         ~ "Right")
  ) %>%
  mutate(Procedure_Type = "I&D") %>%
  sheet_write(ss=ss, sheet="I&D")


  
###############################
###       Wound care        ###
### ----------------------- ###
operation_key <- c("0"="Change",  "1"="Compression", "2"="Dressing", "3"="Immobilization",
                   "4"="Packing", "5"="Removal",     "6"="Traction")
device_key <- c("0"="Traction Apparatus", "1"="Splint", "2"="Cast", "3"="Brace", 
                "4"="Bandage", "5"="Packing Material",  "6"="Pressure Dressing", 
                "7"="Intermittent Pressure Device",     "Y"="Other Device")

pr_ccs %>%
  filter(str_detect(I10_PR, "^2W.[L-V]")) %>% 
  # filter(str_detect(I10_PR, "^2W.[L-V].[4-5]")) %>% 
  select(I10_PR:I10_PR_Desc) %>%
  mutate(Operation = str_sub(I10_PR, start = 3L, end = 3L),
         Operation = recode(Operation, !!!operation_key),
         Region    = str_sub(I10_PR, start = 4L, end = 4L),
         Device    = str_sub(I10_PR, start = 6L, end = 6L),
         Device    = recode(Device, !!!device_key)) %>%
  sheet_write(ss=ss, sheet="WoundCare")
rm(operation_key, device_key)




###############################
###         Smoking         ###
### ----------------------- ###
"F17.2x" %>% 
  
  # Look up codes & children & write to google sheet
  expand_icd10(Comorbidity = "Smoker") %>%
  sheet_write(ss=ss, sheet="Smoker")



###############################
###         Obesity         ###
### ----------------------- ###
icd10_map_ahrq[["Obesity"]] %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Obesity") %>%
  sheet_write(ss=ss, sheet="Obesity")



###############################
###           CKD           ###
### ----------------------- ###
c(icd10_map_ahrq$Renal, "E13.2x") %>% 
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "CKD/ESRD") %>%
  sheet_write(ss=ss, sheet="CKD/ESRD")



###############################
###        Dialysis         ###
### ----------------------- ###
c("N25.0x", "Z49.x", "Z99.2x") %>% 
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Dialysis") %>%
  sheet_write(ss=ss, sheet="Dialysis")
  

  
###############################
###            HTN          ###
### ----------------------- ### 
c(icd10_map_ahrq[["HTN"]], icd10_map_ahrq[["HTNcx"]]) %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "HTN") %>%
  sheet_write(ss=ss, sheet="HTN")



####################################
###   Ophthalmic (Retinopathy)   ###
### ---------------------------- ### 
c("E11.34x", "H350x") %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Ophthalmic (Retinopathy)") %>%
  sheet_write(ss=ss, sheet="Ophthalmic")




###############################
###           PVD           ###
### ----------------------- ###
c(icd10_map_ahrq[["PVD"]], "Z95.9", "E13.51", "E13.59") %>%
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "PVD") %>%
  
  # Flag to indicate if code not included in standard AHRQ set
  mutate(not_in_AHRQ = !ICD10 %in% icd10_map_ahrq[["PVD"]]) %>%
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="PVD")
  
  
  



###############################
###        Neuropathy       ### https://doi.org/10.1016/j.jdiacomp.2017.02.018
### ----------------------- ###
# Codes based on article above & spreadsheet
c("G90.09", "G90.8", "G90.9", "G99.0", 
  "G90.01", "I95.1", "K31.84", "N31.9",
  "E11.4x", "G57.x", "M14.6x") %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Neuropathy") %>%
  sheet_write(ss=ss, sheet="Neuropathy")
  
  

###############################
###           CVA           ###
### ----------------------- ### 
icd10_map_charlson[["Stroke"]] %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "CVA") %>%
  sheet_write(ss=ss, sheet="CVA")



###############################
###           CHF           ###
### ----------------------- ### 
icd10_map_charlson[["CHF"]] %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "CHF") %>%
  sheet_write(ss=ss, sheet="CHF")
  


###############################
###            MI           ###
### ----------------------- ### 
icd10_map_charlson[["MI"]] %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "MI") %>%
  sheet_write(ss=ss, sheet="MI")



###############################
###      Heart Disease      ###
### ----------------------- ### 
c("I20.x", "I25.x", "I47.x", "I48.x", "I49.x") %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Other Heart Dz") %>% 
  sheet_write(ss=ss, sheet="Heart Dz")
  


###############################
###      Osteomyelitis      ###
### ----------------------- ### 
c("M86.16x", "M86.17x", "M86.26x", "M86.27x",
  "M86.66x", "M86.67x") %>%
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "Osteo") %>%
  
  # Split out acuity & location
  separate(long_desc, into = c("Osteo_acuity", "Location"), sep = " osteomyelitis, ") %>%
  
  # Recode acuity
  mutate(Osteo_acuity = recode(Osteo_acuity, 
                               "Other acute"   = "Acute",
                               "Other chronic" = "Chronic")) %>%
  
  # Pull laterality from location
  mutate(Laterality = str_extract(Location, "right |left |unspecified "),
         Location   = str_remove_all(Location, "right |left |unspecified ")) %>%
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="Osteo")

 

###############################
###        Charcot's        ###
### ----------------------- ### 
c("M14.67x", "M14.69") %>%
  
  # Look up codes & children & write to google sheet
  map_dfr(expand_icd10, Comorbidity = "Charcot") %>%
  sheet_write(ss=ss, sheet="Charcot")


###########################################
###   Cellulitis / acute lymphangitis   ###
### ----------------------------------- ### 
c("L03.03x", "L03.115", "L03.116",
  "L03.04x", "L03.125", "L03.126") %>%
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "Skin/SQ infection") %>%
  
  # Split out type & location
  separate(long_desc, into = c("Infxn_type", "Location"), sep = " of ") %>%
  
  # Pull laterality from location
  mutate(Laterality = str_extract(Location, "right|left|unspecified"),
         Location   = str_remove_all(Location, "right |left |unspecified ")) %>%
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="Skin ifxn")



###############################
###          Ulcer          ###
### ----------------------- ### 
c("E11.621", "L97.3x", "L97.4x", "L97.5x") %>%
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "Ulcer") %>%
  
  # Pull laterality
  mutate(Laterality = str_extract(long_desc, "right|left")) %>%
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="Ulcer")



###############################
###       Open wounds       ###
### ----------------------- ### 
"S91.x" %>%
  expand_icd10(Comorbidity = "Open Wounds") %>%
  
  # Unspecified open wound, from external cause
  filter(str_detect(ICD10, "^S91[0-3]0")) %>%
  
  
  # Extract code chars by position
  mutate(n4 = str_sub(ICD10, start = 4L, end = 4L),
         n6 = str_sub(ICD10, start = 6L, end = 6L),
         n7 = str_sub(ICD10, start = 7L, end = 7L),
         n4 = as.numeric(n4),
         n6 = as.numeric(n6)) %>%
  
  
  # Get laterality, location, and timing
  mutate(Laterality = str_extract(long_desc, "right|left")) %>%
  
  mutate(Location = case_when(n4==0             ~ "Ankle",
                              n4==3             ~ "Foot",
                              between(n6, 1, 3) ~ "Great toe(s)",
                              between(n6, 4, 6) ~ "Lesser toe(s)",
                              n6==9             ~ "Unspecified toe(s)",
                              TRUE              ~ "Unspecified toe(s)")) %>%
  
  mutate(Timing = recode(n7, "A" = "Initial encounter", 
                             "D" = "Subsequent encounter",
                             "S" = "sequela")) %>%
  
  # Drop columns
  select(-starts_with("n")) %>%
  
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="Open Wounds")


###############################
###   Stump complications   ###
### ----------------------- ###
c("T87.3x", "T87.4x", "T87.5x", "T87.8x") %>%
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "Stump complications") %>%
  
  # Limit to lower extremity
  filter(str_detect(ICD10, "T87.[3-4]")|str_detect(ICD10, "T878.")) %>%
  
  # Get type of stump complication
  mutate(complication_type = case_when(
                               ICD10=="T8781"            ~ "Dehiscence",
                               ICD10=="T8789"            ~ "Other",
                               str_detect(ICD10, "T873") ~ "Neuroma",
                               str_detect(ICD10, "T874") ~ "Infection",
                               str_detect(ICD10, "T875") ~ "Necrosis")) %>%
  
  # Pull laterality
  mutate(Laterality = case_when(str_detect(ICD10, "T87.3") ~ "Right",
                                str_detect(ICD10, "T87.4") ~ "Left")) %>%
  
  # Write to google sheet
  sheet_write(ss=ss, sheet="Stump complications")
  
  





## These CPT codes below map to the CCS
# CPT codes: c(11042:11047, 97597:97602, 10060, 10061, 20000, 20005)

## CSS:
# > 164	Other OR therapeutic procedures on musculoskeletal system
# > 168	Incision and drainage, skin and subcutaneous tissue			
# > 169	Debridement of wound, infection or burn			
# > 214	Traction, splints, and other wound care			
pr_ccs %>%
  filter(CCS_category %in% c(164, 169)) %>%
  # filter(CCS_category == "169") %>%
  mutate(asd = str_sub(I10_PR, start = 1L, end = 2L)) %>%
  filter(!str_detect(I10_PR, "^0X")) %>%
  # count(asd) 
  filter(str_detect(I10_PR, "^0Y")) %>%
  View()







###############################
###       Other ideas       ###
### -------+-------+------- ###

# c("E11.52x", "A48.0") %>% map_dfr(expand_icd10, Comorbidity = "Gangrene") %>%
#   View()

# c("T81.4x") %>% map_dfr(expand_icd10, Comorbidity = "SSI") %>%
#   View()

ccs_pr_icd10 %>%
  filter(CCS_category %in% c(60, 142, 161,164, 168, 170, 171, 173, 174, 175, 214, 222)) %>%
  filter(!str_detect(CCS_category_Desc, "bone|Bone")) %>%
  filter(!CCS_category %in% c(170, 168, 214, 222)) %>%
  count(CCS_category, CCS_category_Desc) %>%
  # filter(CCS_category == 157) %>%
  View()

ccs_pr_icd10 %>%
  filter(str_detect(I10_PR, "^2W.[L-V]")) %>%
  View()


ccs_pr_icd10 %>%
  filter(str_detect(I10_PR, "^F.8L")) %>%
  View()


ccs_pr_icd10 %>%
  filter(str_detect(I10_PR, "^0J.[L-R]")) %>%
  View() # see https://icdlist.com/icd-10-pcs/0JCQ0ZZ

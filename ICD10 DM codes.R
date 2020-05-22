library(tidyverse)
library(icd)    # helps with ICD codes 
# devtools::install_github("jackwasey/icd")
library(touch)  # Tools Of Unilization and Cost in Healthcare
library(googlesheets4)


###############################
###        Read files       ###
### ----------------------- ###
ss <- gs4_get("https://docs.google.com/spreadsheets/d/166lGOoZZFeuD4eBXyZQVkWyYw0XnkwhbAsQEEEQ-I7c/edit#gid=0")
pr_ccs <- read_csv("HCUP Files/clean_ccs/ccs_pr_icd10.csv")




###################################################################
###   Function that given ICD-10 codes, will return             ###
###   a table with their descriptions. Additionally, this       ###
###   expands any children codes if the ICD-10 code ends in x   ###
### ----------------------------------------------------------- ###
expand_icd10 <- function(single_code, ..., verbose=T){
  dots <- list(...)
  
  # Should be a single code
  testthat::expect_length(single_code, 1)
  
  # Logical flag if children should be returned
  get_children <- str_detect(single_code, "x$")
  
  # If present, remove the x and make short code
  parent <- as.icd10(str_remove(single_code, "x$")) %>% decimal_to_short()
  
  # Give warning if code not found
  if(!is_defined(parent)) warning("parent code `",parent, "` not defined ICD-10 code")
  
  # New object called target codes
  target_codes <- parent
  
  if(get_children){
    target_codes <- parent %>% children()
    if(length(target_codes)>1 & verbose==T) message(length(target_codes)-1,
                                                    " children codes found for ", single_code)
  }
  
  
  # Make the table
  df <- target_codes %>%
    explain_table() %>% as_tibble() %>%
    mutate(code_decimal = short_to_decimal(code),
           parent_code  = single_code) %>%
    select(parent_code, ICD10=code, code_decimal, short_desc, long_desc) %>%
    mutate_all(as.character) %>%
    
    mutate(...)  # forward tidy dots
  
    df %>% select(names(dots), parent_code:long_desc) # reorder
  
}






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
# Codes for CKD from AHRQ  -plus-  diabetes CKD (E13.2x)  -plus- dialysis codes
c(icd10_map_ahrq$Renal, "E13.2x", "N25.0x", "Z49.x", "Z99.2x") %>% 
  
  # Look up codes & children
  map_dfr(expand_icd10, Comorbidity = "CKD/ESRD") %>%
  
  # Flags to indicate if dependant on dialysis & if code is
  # not included in standard AHRQ set
  mutate(
    Dialysis = ICD10 %in% children(c("N250", "Z49", "Z992")),
    not_in_AHRQ = !ICD10 %in% icd10_map_ahrq[["Renal"]]) %>%
  
  # Write to googlesheet
  sheet_write(ss=ss, sheet="CKD/Dialysis")
  

  
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

### Osteomyelitis
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
###       Other ideas       ###
### -------+-------+------- ###

# c("S91.10x", "S91.20x", "S91.30x") %>% map_dfr(expand_icd10, Comorbidity = "Open wound") %>%
#   View()

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

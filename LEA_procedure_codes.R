library(tidyverse)
library(icd)    # helps with ICD codes 

pr_ccs <- read_csv("https://github.com/HunterRatliff1/HCUP/raw/master/HCUP%20Files/clean_ccs/ccs_pr_icd10.csv", 
                   col_types = cols(.default = col_character())) %>%
  select(I10_PR:I10_PR_Desc) 

# Load helper functions, specifically "split_pcs10_codes"
source("~/Github/HCUP/helper_functions.R") 
# See https://raw.githubusercontent.com/HunterRatliff1/HCUP/master/helper_functions.R





#############################
###   Lower Extremities   ###
### --------------------- ###
LE_recode <- list(
  "BodySystem" = c("H"="Skin", 
                   "J"="SQ/Fascia", 
                   "Y"="Lower Extremities"),
  "Operation"  = c("0" = "Alteration", "2" = "Change",     "3" = "Control",
                   "6" = "Detachment", "9" = "Drainage",   "B" = "Excision",
                   "H" = "Insertion",  "J" = "Inspection", "M" = "Reattachment",
                   "P" = "Removal",    "Q" = "Repair",     "U" = "Supplement",
                   "W" = "Revision"),
  "BodyPart"   = c("0"="R Buttock",         "1"="L Buttock",
                   "2"="R Hindquarter",     "3"="L Hindquarter",     "4"="B Hindquarter",  
                   "5"="R Inguinal Region", "6"="L Inguinal Region", "A"="B Inguinal Region",
                   "7"="R Femoral Region",  "8"="L Femoral Region",  "E"="B Femoral Region", 
                   "9"="R Low Extrem",      "B"="L Low Extrem",
                   "C"="R Upper Leg",       "D"="L Upper Leg",
                   "F"="R Knee",            "G"="L Knee",
                   "H"="R Lower Leg",       "J"="L Lower Leg",
                   "K"="R Ankle",           "L"="L Ankle",
                   "M"="R Foot",            "N"="L Foot",
                   "P"="R Toe_1",           "Q"="L Toe_1",
                   "R"="R Toe_2",           "S"="L Toe_2",
                   "T"="R Toe_3",           "U"="L Toe_3",
                   "V"="R Toe_4",           "W"="L Toe_4",
                   "X"="R Toe_5",           "Y"="L Toe_5"),
  "Approach"   = c("0"="Open", "3"="Percutaneous", "4"="Perc Endo", "X"="External"),
  "Device"     = c("Z"="", "0"="Drainage Device",
                   "1"="Radioactive Element", "3"="Infusion Device",
                   "7"="Autologous Tissue Substitute",
                   "J"="Synthetic Substitute",
                   "K"="Nonautologous Tissue Substitute", "Y"="Other Device"),
  "Qualifier"  = c("Z"="", "X"="Diagnostic",
                   "0"="Complete", "1"="High", "2"="Mid", "3"="Low",
                   
                   "4"="Ray_Complete_1",  "9"="Ray_Partial_1",
                   "5"="Ray_Complete_2",  "B"="Ray_Partial_2",
                   "6"="Ray_Complete_3",  "C"="Ray_Partial_3",
                   "7"="Ray_Complete_4",  "D"="Ray_Partial_4",
                   "8"="Ray_Complete_5",  "F"="Ray_Partial_5")
)

 

LE_df <- pr_ccs %>% 
  filter(str_detect(I10_PR, "^0Y")) %>% 
  
  # Split the codes up and recode with provided list
  split_pcs10_codes(recode_list = LE_recode) %>%
  
  mutate(Laterality = str_extract(BodyPart, "^[R|L|B](?= )"),
         Laterality = recode(Laterality, "B"="Bilateral"),
         BodyPart   = str_remove(BodyPart, "^[R|L|B] ")) 
rm(LE_recode)  


# LE_df %>%
#   filter(Qualifier!="Diagnostic") 
  
  
  
  







############################
###   Draining of Skin   ###
### -------------------- ###
# Section 0H9
skin_recode <- list(
  "BodySystem" = c("H"="Skin", 
                   "J"="SQ/Fascia", 
                   "Y"="Lower Extremities"),
  "Operation"  = c("9"="Drainage"),
  "BodyPart"   = c("8" = "Buttock", "A"="Ingunal",
                   "H" = "R Upper Leg", "J" = "L Upper Leg",
                   "K" = "R Lower Leg", "L" = "L Lower Leg",
                   "M" = "R Foot",      "N" = "L Foot"),
  "Approach"   = c("X"="External"),
  "Device"     = c("0"="Drainage Device", "Z"=""),
  "Qualifier"  = c("X"="Diagnostic", "Z"="")
)

skin_df <- pr_ccs %>% 
  filter(str_detect(I10_PR, "^0H9[8|A|H-N]")) %>% 
  
  # Split the codes up and recode with provided list
  split_pcs10_codes(recode_list = skin_recode) %>% 
  
  mutate(Laterality = str_extract(BodyPart, "^[R|L|B](?= )"),
         Laterality = recode(Laterality, "B"="Bilateral"),
         BodyPart   = str_remove(BodyPart, "^[R|L|B] ")) 
rm(skin_recode)

skin_df %>%
  filter(Qualifier!="Diagnostic") 

  

#################################
###   Draining of SQ/Fascia   ###
### ------------------------- ###
# Section 0J9
SQ_recode <- list(
  "BodySystem" = c("H"="Skin", 
                   "J"="SQ/Fascia", 
                   "Y"="Lower Extremities"),
  
  "Operation"  = c("9"="Drainage"),
  
  "BodyPart"   = c("9" = "Buttock", 
                   "L" = "R Upper Leg", "M" = "L Upper Leg",
                   "N" = "R Lower Leg", "P" = "L Lower Leg",
                   "Q" = "R Foot",      "R" = "L Foot"),
  
  "Approach"   = c("0"="Open", "3"="Percutaneous"),
  
  "Device"     = c("0"="Drainage Device", "Z"=""),
  
  "Qualifier"  = c("X"="Diagnostic", "Z"="")
)

SQ_df <- pr_ccs %>% 
  filter(str_detect(I10_PR, "^0J9[9|L-R]")) %>% 
  
  # Split the codes up and recode with provided list
  split_pcs10_codes(recode_list = SQ_recode) %>%
  
  mutate(Laterality = str_extract(BodyPart, "^[R|L|B](?= )"),
         Laterality = recode(Laterality, "B"="Bilateral"),
         BodyPart   = str_remove(BodyPart, "^[R|L|B] ")) 
rm(SQ_recode)


#################################
###        Wound Care         ###
### ------------------------- ###
# Section 2W
WC_recode <- list(
  "BodySystem" = c("W"="Placement, Anatomical Regions"),
  "Operation"  = c("0"="Change",  "1"="Compression", "2"="Dressing", "3"="Immobilization", 
                   "4"="Packing", "5"="Removal",     "6"="Traction"),
  "BodyPart"   = c("6"="R Inguinal Region", "7"="L Inguinal Region",
                   "L"="R Low Extrem",      "M"="L Low Extrem",
                   "N"="R Upper Leg",       "P"="L Upper Leg",
                   "Q"="R Lower Leg",       "R"="L Lower Leg",
                   "S"="R Foot",            "T"="L Foot",
                   "U"="R Toe",           "V"="L Toe"),
  "Approach"   = c("X"="External"),
  "Device"     = c("Z"=" ", "Y"="Other Device", "0"="Traction Apparatus", 
                   "1"="Splint", "2"="Cast", "3"="Brace", "4"="Bandage", 
                   "5"="Packing Material",  "6"="Pressure Dressing", 
                   "7"="Intermittent Pressure Device"),
  "Qualifier"  = c("Z"="")
)


wc_df <- pr_ccs %>%
  # filter(str_detect(I10_PR, "^2W.[L-V]")) %>%
  filter(str_detect(I10_PR, "^2W.[6-7|L-V]")) %>%
  
  ## Column headings aren't technically correct, but still follow same
  ## principles. See links below for details
  ## > https://icdlist.com/icd-10-pcs/section/2-placement
  ## > https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-pcs-procedure-coding-system.pdf#page=15
  split_pcs10_codes(recode_list = WC_recode) %>%
  
  mutate(Laterality = str_extract(BodyPart, "^[R|L|B](?= )"),
         Laterality = recode(Laterality, "B"="Bilateral"),
         BodyPart   = str_remove(BodyPart, "^[R|L|B] ")) 
rm(WC_recode)




###############################
###   Write to Excel file   ###
### ----------------------- ###
drainage <- bind_rows(SQ_df, skin_df, filter(LE_df, Operation=="Drainage")) 
LEA <- LE_df %>% filter(Operation=="Detachment")
all_other_LE_proc <- LE_df %>% filter(!Operation %in% c("Detachment", "Drainage"))

# export
list(
  "I&D"                 = drainage,
  "LEA"                 = LEA,
  "WoundCare"           = wc_df,
  "Other LE procedures" = all_other_LE_proc
) %>%
  WriteXLS::WriteXLS(ExcelFileName = "LEA_procedure_codes.xlsx")

rm(LEA, all_other_LE_proc, SQ_df, skin_df)


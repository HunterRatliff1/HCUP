## https://www.hcup-us.ahrq.gov/db/nation/nis/nisdde.jsp
# hcupnote p
# .pagetext table
get_format <- function(table_name, year=2012, 
                       print=FALSE, ...) {
  
  library(tidyverse)
  dots <- list(...)
  
  
  
  #####################################################################
  ###   Build out list of valid tables, given the year. There are   ###
  ###   a few inconsistencies, outlined here:                       ###
  ###    - 2004 and earlier have weird table names                  ###
  ###    - 2015 has Q4 split out from Q1 - Q3 for the Severity      ###
  ###      and DX_PR_GRPS files                                     ###
  ###    - For 2016 & 2017, there aren't any Diagnosis and          ###
  ###      Procedure Groups Files (DX_PR_GRPS)                      ###
  ###    - Between 2005 & 2007, the DX_PR_GRPS has a different      ###
  ###      capitalization (Dx_Pr_Grps)                              ###
  ### ------------------------------------------------------------- ###
  if(!between(year, 2005, 2017)) {
    year <- readline("Enter a year between 2005 & 2017: ")  
  }
  # Base set of options
  table_name_options <- c("Core", "Hospital", "Severity", "DX_PR_GRPS")
  
  if(year==2015) table_name_options <- c("Core", "Hospital", 
                                         "Q1Q3_Severity", "Q1Q3_DX_PR_GRPS",
                                         "Q4_Severity",   "Q4_DX_PR_GRPS")
  
  if(year==2016|year==2017) table_name_options <- c("Core", "Hospital", "Severity")
  
  if(between(year, 2005, 2007)) table_name_options <- c("Core", "Hospital", "Severity", "Dx_Pr_Grps")
  
  
  
  
  
  
  #########################################################################
  ###   When no table name is given, prompt the user (if interactive)   ###
  ###   or assign default name (if not interactive). Additionally,      ###
  ###   if an invalid name is given, as may be the case in 2015,        ###
  ###   prompt the user to enter the correct name                       ###
  ### ----------------------------------------------------------------- ###
  if(missing(table_name)) {
    if(interactive()){
      name_index <- menu(choices = table_name_options, title="Which table should be used?")
      table_name <- table_name_options[name_index]
    } else({
      table_name <- "Hospital"
      message("No `table_name` supplied. Defaulting to 'Hospital'")
    })
  }
  
  # Allow tables to be referenced by index
  if(table_name %in% 1:4){
    table_name <- table_name_options[table_name]
  }
  
  # If invalid table name given, prompt new one
  file <- table_name
  if(!file %in% table_name_options){
    if(interactive()){
      name_index <- menu(choices = table_name_options, 
                         title = paste0("The specified table does not exist for ",year,". Please select from below"))
      table_name <- table_name_options[name_index]
      file <- table_name
    } else(warning("`table_name` must be one of: ", str_c(table_name_options, collapse = ", ")))
    
  }
  

  
  
  #####################################################################
  ###   This gets the text file, which we need to parse in future   ###
  ###   steps. This text file has a header and two tables. The      ###
  ###   first table specifies the fwf columns which is used to      ###
  ###   read in the second table                                    ###
  ### ------------------------------------------------------------- ###
  path <- paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_",
                 year, "_", file,".TXT")
  if(year==2015 & str_detect(file, "^Q")) {
    # The quarterly files have different URLs
    path <- paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_",
                   year, file,".TXT")
  }
  
  if(httr::http_error(path)) {
    warning("Invalid URL. For details see\n https://www.hcup-us.ahrq.gov/db/nation/nis/nisfilespecs.jsp")  
    return(NULL)
  }
  fmt_txt <- path %>% read_lines()
  
  
  
  #####################################################################
  ###   Read in the positions of the columns in the second table,   ### 
  ###   using data from the first table                             ###
  ### ------------------------------------------------------------- ###
  t1_start <- str_which(fmt_txt, "Columns") + 1
  t1_end   <- str_which(fmt_txt, "Data element label")
  
  t1 <- read_fwf(path, skip = t1_start, n_max = t1_end - t1_start,col_types = "cc",
                 col_positions = fwf_widths(c(8, NA), col_names = c("Columns","Description")))
  
  t1 <- t1 %>% 
    mutate(Columns = str_remove_all(Columns, " ")) %>%
    tidyr::separate(Columns, into=c("start", "end"), fill="right", convert=T) %>%
    
    # if only one number, repeat that one for the end
    mutate(end = if_else(is.na(end), start, end)) %>%
    # handle ragged endings
    mutate(end = ifelse(end==max(end), NA, end))
  
             
  ####################################################
  ###           Read in the second table           ### 
  ### -------------------------------------------- ###
  t2_start <- min(str_which(fmt_txt, "^NIS"))
  # c(t2_start, t1_end+3)
             
  t2 <- read_fwf(path, skip = t2_start - 1, col_types = cols(.default = col_character()),
                 col_positions = fwf_positions(start = t1$start, end = t1$end, col_names = t1$Description))
  
  if(print) print(fmt_txt)
  
  # Return the table
  invisible(t2)
  
  # dots
}
get_format("Severity", 2015, Q4=F)
get_format("Severity", 2003)
get_format("DX_PR_GRPS", 2006)


core <- 2005:2017 %>%
  map_dfr(~get_format("Core", .x))

hospital <- 2005:2017 %>%
  map_dfr(~get_format("Hospital", .x))

names(hospital)
names(core)

severity <- c(2005:2015,2015:2017) %>%
  map(~get_format("Severity", .x))

DxPrGrp <- c(2005:2015, 2015) %>%
  map(~get_format("DX_PR_GRPS", .x))

severity_core %>%
  map(names) 

x <- 2005:2017 %>%
  map_dfr(~get_format("Core", .x))











  
  
  
  
# library(SAScii)
# 
# SAScii::parse.SAScii("~/NIS Source/2012 files/SAS read files/SASLoad_NIS_2012_Hospital.SAS", beginline = 187)
# rstudioapi::selectFile()
# 
# # format  <-
# 
# paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/pgms/SASLoad_NIS_", 2012, "_Hospital.SAS") %>%
#   # paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/pgms/StataLoad_NIS_", 2012, "_Hospital.Do") %>%
#   
#   readLines()


# library(tidyverse)
# library(haven)
# nis_2012_severity <- read_sas("/Volumes/32GB SD/NIS 2012/nis_2012_severity.sas7bdat", 
#                               NULL)
# sjlabelled::get_label(nis_2012_severity) %>% str()
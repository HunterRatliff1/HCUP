expand_icd10 <- function(single_code, ..., verbose=T){
  ### ---------------------- DESCRIPTION ---------------------- ###
  ###   Function that given ICD-10-CM codes, will return a table
  ###   with their descriptions. Additionally, this expands any 
  ###   children codes if the ICD-10 code ends in 'x'.
  ###
  ###   NOTE: this will not work ICD-10-PCS codes (procedure codes)
  ###         or with ICD-9 codes
  ###
  ###   This only handles single codes, but can be easily mapped
  ###   using purrr:
  ###
  ###   > c("N25.0x", "Z49.x", "Z99.2x") %>% 
  ###   +     map_dfr(expand_icd10, Comorbidity = "Dialysis")
  
  # Required packages
  library(icd)
  library(tidyverse)
  
  dots <- list(...)
  
  # Should be a single code
  testthat::expect_length(single_code, 1)
  
  # Logical flag if children should be returned
  get_children <- str_detect(single_code, "x$")
  
  # If present, remove the x and make short code
  parent <- as.icd10(str_remove(single_code, "x$")) %>% decimal_to_short()
  
  # Give warning if code not found
  if(!is_defined(parent)) warning("parent code `",parent, "` not defined ICD-10-CM code")
  
  
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



split_pcs10_codes <- function(.data, ..., recode_list, as_fct=F, col_name=I10_PR){
  ### Splits ICD-10 procedure codes into their components. Codes must
  ### be contained in a data.frame/tibble (`.data`) in the column 
  ### specified by `I10_PR` (unquoted).
  ###
  ### If given a list in `recode_list`, it will recode these columns
  ### with the arguments in the list
  dots <- list(...)
  col_name <- enquo(col_name)
  
  # split out the codes
  df <- .data %>%
    mutate(BodySystem = str_sub(as.character(!!col_name), start = 2L, end = 2L),
           Operation  = str_sub(as.character(!!col_name), start = 3L, end = 3L),
           BodyPart   = str_sub(as.character(!!col_name), start = 4L, end = 4L),
           Approach   = str_sub(as.character(!!col_name), start = 5L, end = 5L),
           Device     = str_sub(as.character(!!col_name), start = 6L, end = 6L),
           Qualifier  = str_sub(as.character(!!col_name), start = 7L, end = 7L))
  
  # If given list to recode
  if(!missing(recode_list)) {
    testthat::expect_named(recode_list, c("BodySystem", "Operation", "BodyPart",
                                          "Approach", "Device", "Qualifier"))
    
    df <- df %>%
      mutate(BodySystem = recode(BodySystem, !!!recode_list$BodySystem),
             Operation  = recode(Operation,  !!!recode_list$Operation),
             BodyPart   = recode(BodyPart,   !!!recode_list$BodyPart),
             Approach   = recode(Approach,   !!!recode_list$Approach),
             Device     = recode(Device,     !!!recode_list$Device),
             Qualifier  = recode(Qualifier,  !!!recode_list$Qualifier))
  }
  
  # If told to make factors
  if(as_fct) df <- mutate_at(df, vars(BodySystem:Qualifier), as_factor)
  
  df
  
}



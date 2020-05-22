library(rvest)
library(tidyverse)

# Get codebook table
codebook_tbl <- "https://www.hcup-us.ahrq.gov/db/nation/nis/nisdde.jsp" %>%
  read_html(encoding = "ISO-8859-1") %>%
  html_node("table.pagetext") %>%
  html_table() %>%
  as_tibble() %>%
  
  # Drop rows spanning multiple columns
  filter(!`Descriptive Title`==Years, !`File(s) in 2015`==Years)

# Make names better
names(codebook_tbl) <- names(codebook_tbl) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("\\(s\\)","s") 

codebook_tbl %>%
  mutate(Q_2015 = str_extract(Years, "Q."),
         yrs = Years,
         yrs = str_remove(yrs, " Q."),
         yrs = str_replace(yrs, "2015, 2016-", "2015-"),
         yrs = str_replace(yrs, "-2015, 2016", "-2016"),
         yrs = str_replace(yrs, "2015, 2016", "2015-2016"),
         yrs = ifelse(yrs=="2000", "2000-2000", yrs)) %>% 
  
  # Split the one row with multiple years
  tidyr::separate(yrs, c("yrs", "yrs2"), 
                  sep=", ", fill="right") %>%  
  pivot_longer(cols=yrs:yrs2, values_to = "yrs") %>%
  filter(!is.na(yrs)) %>% select(-name) %>%
  
  mutate(yr_start = str_extract(yrs, ".+(?=-)"),
         yr_end   = str_extract(yrs, "(?<=-).+")) %>%
  
  
  View()




read_html("https://www.hcup-us.ahrq.gov/db/vars/nis_stratum/nisnote.jsp", encoding = "ISO-8859-1") %>%
  # guess_encoding()
  html_node("upnote p")
html_session("https://www.hcup-us.ahrq.gov/db/vars/nis_stratum/nisnote.jsp")

"https://stats.idre.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-a-stratified-design-with-certainty-psus/" %>%
  read_html()

"1988-1997" %>% str_extract("(?<=-).+")


library(tidyverse)
library(lubridate)
library(xml2)

#function to convert html ecoded characters to text
html2txt <- function(str) {
  xml_text(read_html(paste0("<x>", str, "</x>"))) #create xml node to be read as html and converted to text
}

replace_special <- function(x){  #function to replace special characters with standard alphabet letters
  case_when(#these regex expressions won't work when running R on windows
    str_detect(x, fixed("\xf6")) ~ str_replace(x, fixed("\xf6"), "o"), #replace with "o"
    str_detect(x, fixed("\xfc")) ~ str_replace(x, fixed("\xfc"), "u"), #replace with "u"
    str_detect(x, "&amp;") ~ str_replace(x, "&amp;", "and"), #replace with "and"
    str_detect(x, "&apos;") ~ str_replace(x, "&apos;", "'"), #replace with apostrophes
    str_detect(x, "&#x[:alnum:]*;") ~ paste(html2txt(x)), #fix html-encoded characters
    TRUE ~ paste(x)) #keep original value otherwise
}

test_data <- read_csv("processed_data/test_report_data.csv")

test_data_ed <- test_data %>% unite(., Editor, first.name, last.name, sep = " ") %>% 
  mutate(Editor = map(Editor, replace_special))

clean_test_data <- test_data_ed %>% 
  select(-middle.name) %>% 
  mutate(Editor = unlist(Editor)) %>% 
  mutate(`Date of Publication` = mdy(`Date of Publication`)) %>% 
  rename(., "editor" = "Editor", "publication.date" = "Date of Publication", "measure.names" = "Measure Names",
         "measure.values" = "Measure Values", "months.published" = "Published Months", "ejp.decision" = "EJP.decision")

write_csv(clean_test_data, "processed_data/clean_test_data.csv")

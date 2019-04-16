library(tidyverse)
library(lubridate)#convert date formats
library(xml2)#for html2txt function

#Relevant Functions----
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

#Load datsets----
manu_data <- report_parse %>% 
  mutate(doi = tolower(doi)) %>% select(-related.manu, -is.resubmission) %>% 
  filter(journal != "EC") %>% filter(journal != "genomeA") %>% filter(journal != "CVI")
  
#people_data <- read_csv("processed_data/2018_people.csv") %>% 
#  filter(role == "editor"|role == "senior.editor") %>% 
#  select(person.id, manuscript.number, contains("name"), role) %>% 
#  distinct()

#review_data <- read_csv("processed_data/2018_reviews.csv")

usage_files <- list.files("processed_data/usage", full.names = TRUE) #new method will have single dataset -- change to reflect 

usage_data <- map_df(usage_files, read_csv)

usage_select <- usage_data %>% select(`Article Date of Publication (article_metadata)`, `Article DOI (article_metadata)`, `Measure Names`,
                                      `Measure Values`, `Published Months`)

citation_files <- list.files("processed_data/citations", full.names = TRUE)
#new method will have single dataset -- change to reflect 
citation_data <- map_df(citation_files, read_csv)

citation_select <- citation_data %>% select(`Article DOI (article_metadata)`, `Date of Publication`, `Measure Names`, `Measure Values`, `Published Months`)

published_data <- full_join(citation_select, usage_select, by = c("Date of Publication" = "Article Date of Publication (article_metadata)", "Published Months", "Measure Names", "Measure Values", "Article DOI (article_metadata)"))

report_data <- left_join(manu_data, published_data, by = c("doi" = "Article DOI (article_metadata)"))

report_data <- rename(report_data, "editor.id" = "person.id.x", "reviewer.id" = "person.id.y") #not sure what to do here

#clean merged datasets-----
report_data_ed <- report_data %>% unite(., Editor, first.name, last.name, sep = " ") %>% 
  mutate(Editor = map(Editor, replace_special))

clean_report_data <- report_data_ed %>% 
  select(-middle.name) %>% 
  mutate(Editor = unlist(Editor)) %>% 
  mutate(`Date of Publication` = mdy(`Date of Publication`)) %>% 
  rename(., "editor" = "Editor", "publication.date" = "Date of Publication", "measure.names" = "Measure Names",
         "measure.values" = "Measure Values", "months.published" = "Published Months", "ejp.decision" = "EJP.decision")

write_csv(report_data, paste0("processed_data/report_data", this_ym,".csv"))


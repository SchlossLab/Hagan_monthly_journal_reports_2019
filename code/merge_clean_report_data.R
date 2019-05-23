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
  filter(manuscript.number != "NA") %>% 
  filter(journal != "EC") %>% filter(journal != "genomeA") %>% filter(journal != "CVI")
  
#people_data <- read_csv("processed_data/2018_people.csv") %>% 
#  filter(role == "editor"|role == "senior.editor") %>% 
#  select(person.id, manuscript.number, contains("name"), role) %>% 
#  distinct()

#review_data <- read_csv("processed_data/2018_reviews.csv")

usage_data <-  read_csv("processed_data/usage.csv")

usage_select <- usage_data %>% select(`Article Date of Publication (article_metadata)`, `Article DOI (article_metadata)`, 
                                      `Total Abstract`, `Total HTML`, `Total PDF`)

citation_data <- read_csv("processed_data/cites.csv")

citation_select <- citation_data %>% select(`Article DOI (article_metadata)`, `Article Date of Publication (article_metadata)`, 
                                            Cites, `Citation Date`, `Published Months`) %>% 
  filter(Cites != 0) %>%
  group_by(`Article DOI (article_metadata)`, `Article Date of Publication (article_metadata)`, `Published Months`) %>% 
  summarise(Cites = n())

jif_data <- citation_data %>% select(`Article DOI (article_metadata)`, Cites, `Citation Date`, 
                                     `Article Date of Publication (article_metadata)`) %>% 
  filter(Cites != 0) %>% distinct()

published_data <- full_join(citation_select, usage_select, 
                            by = c("Article Date of Publication (article_metadata)", 
                                   "Article DOI (article_metadata)")) %>% distinct()

jif_report_data <- manu_data %>% 
  filter(!is.na(doi)) %>% 
  select(doi, manuscript.type, journal) %>% 
  left_join(., jif_data, by = c("doi" = "Article DOI (article_metadata)")) %>% distinct()

report_data <- left_join(manu_data, published_data, by = c("doi" = "Article DOI (article_metadata)"))

#clean merged datasets-----
report_data_ed <- report_data %>% unite(., Editor, first.name, last.name, sep = " ") %>% 
  mutate(Editor = map(Editor, replace_special))

clean_report_data <- report_data_ed %>% 
  #select(-middle.name) %>% 
  mutate(Editor = unlist(Editor)) %>% 
  mutate(`Article Date of Publication (article_metadata)` = mdy(`Article Date of Publication (article_metadata)`)) %>% 
  rename(., "editor" = "Editor", "publication.date" = "Article Date of Publication (article_metadata)", 
         "months.published" = "Published Months", "ejp.decision" = "EJP.decision", "Total Article Cites"= "Cites",
         "Abstract" = "Total Abstract", "HTML" = "Total HTML", "PDF" = "Total PDF") %>% 
  gather(`Total Article Cites`:PDF, key = measure.names, value = measure.values)

write_csv(report_data, paste0("processed_data/report_data", this_ym,".csv"))


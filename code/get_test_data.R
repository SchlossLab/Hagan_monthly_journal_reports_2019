library(tidyverse)
library(stringr)

manu_data <- read_csv("processed_data/all_manu_all_xml.csv") %>% 
  mutate(doi = tolower(doi))

people_data <- read_csv("processed_data/all_people_all_xml.csv") %>% 
  filter(role == "editor"|role == "senior.editor") %>% 
  select(person.id, manuscript.number, first.name, role) %>% 
  distinct()

published_data <- read_csv("processed_data/all_published.csv")

report_data <- left_join(manu_data, people_data, by = "manuscript.number") %>% 
  left_join(., published_data, by = c("doi" = "Article DOI (article_metadata)"))

write_csv(report_data, "processed_data/test_report_data.csv")


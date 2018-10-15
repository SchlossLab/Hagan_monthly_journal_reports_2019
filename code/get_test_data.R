library(tidyverse)

manu_data <- read_csv("processed_data/2018_grouped_manu_fixed.csv") %>% 
  mutate(doi = tolower(doi))

manu_data_clean <- manu_data %>% select(-related.manu, -is.resubmission)

people_data <- read_csv("processed_data/2018_people.csv") %>% 
  filter(role == "editor"|role == "senior.editor") %>% 
  select(person.id, manuscript.number, contains("name"), role) %>% 
  distinct()

review_data <- read_csv("processed_data/2018_reviews.csv")

usage_files <- list.files("processed_data/usage", full.names = TRUE)

usage_data <- map_df(usage_files, read_csv)

usage_select <- usage_data %>% select(`Article Date of Publication (article_metadata)`, `Article DOI (article_metadata)`, `Measure Names`,
                                      `Measure Values`, `Published Months`)

citation_files <- list.files("processed_data/citations", full.names = TRUE)

citation_data <- map_df(citation_files, read_csv)

citation_select <- citation_data %>% select(`Article DOI (article_metadata)`, `Date of Publication`, `Measure Names`, `Measure Values`, `Published Months`)

published_data <- full_join(citation_select, usage_select, by = c("Date of Publication" = "Article Date of Publication (article_metadata)", "Published Months", "Measure Names", "Measure Values", "Article DOI (article_metadata)"))

report_data <- left_join(manu_data_clean, people_data, by = "manuscript.number") %>% 
  left_join(., review_data, by = "manuscript.number") %>% 
  left_join(., published_data, by = c("doi" = "Article DOI (article_metadata)"))

report_data <- rename(report_data, "editor.id" = "person.id.x", "reviewer.id" = "person.id.y")

write_csv(report_data, "processed_data/test_report_data.csv")


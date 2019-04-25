

data <- read_csv("processed_data/report_parse_2019-04.csv") %>% 
  rename(transfer.type = transfer_type, number.authors = number_authors) %>% 
  mutate(doi = tolower(doi)) %>% select(-related.manu, -is.resubmission) %>% 
  filter(journal != "EC") %>% filter(journal != "genomeA") %>% filter(journal != "CVI") %>% 
  unite(., Editor, first.name, last.name, sep = " ") %>% 
  mutate(Editor = map(Editor, replace_special)) %>% 
  mutate(Editor = unlist(Editor)) 

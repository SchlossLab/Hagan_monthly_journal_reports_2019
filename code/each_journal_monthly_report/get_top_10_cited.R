#10 most frequently read articles published in the last 12 months, ranked on full-text & pdf views

top_cited_data <- data %>% filter_12_to_36_mo(., .$publication.date) %>% 
  filter(journal == this_journal) %>% 
  filter(measure.names == "Total Article Cites") %>% 
  filter(role == "editor") %>% 
  select(editor, doi, title, publication.date, manuscript.type, category, measure.names, measure.values) %>% 
  distinct() %>% select(-measure.names)

top_cited_url <- top_cited_data %>% arrange(desc(measure.values)) %>% 
  head(n = 10) %>% 
  mutate(url = paste0("https://www.dx.doi.org/", doi)) %>% 
  pull(url)

top_cited_summary <- top_cited_data %>% 
  arrange(desc(measure.values)) %>% 
  select(-doi) %>% 
  head(n = 10) %>%
  mutate(title = str_to_title(title)) %>% 
  mutate(title = cell_spec(title, "html", link = top_cited_url))

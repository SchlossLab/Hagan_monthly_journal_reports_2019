#10 most frequently read articles published in the last 12 months, ranked on full-text & pdf views

top_data <- data %>% filter_12_mo(., .$publication.date) %>% 
  filter(journal == this_journal) %>% 
  filter(measure.names == "PDF" | measure.names == "HTML") %>% 
  filter(role == "editor") %>% 
  select(editor, title, doi, publication.date, manuscript.type, category, measure.names, measure.values) %>% 
  distinct() %>% 
  spread(., key = measure.names, value = measure.values) %>% 
  mutate(total_reads = HTML + PDF)

top_url <- top_data %>% arrange(desc(total_reads)) %>% 
  select(-HTML, -PDF) %>% head(n = 10) %>% 
  mutate(url = paste0("https://www.dx.doi.org/", doi)) %>% 
  pull(url)

top_summary <- top_data %>% arrange(desc(total_reads)) %>% 
  select(-HTML, -PDF, -doi) %>% head(n = 10) %>% 
  mutate(title = str_to_title(title)) %>% 
  mutate(title = cell_spec(title, "html", link = top_url))

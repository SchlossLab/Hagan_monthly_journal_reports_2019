#10 most frequently read articles published in the last 12 months, ranked on full-text & pdf views

top_data <- data %>% filter_12_mo(., .$publication.date) %>% 
  filter(journal == this_journal) %>% 
  filter(measure.names == "PDF" | measure.names == "HTML") %>% 
  filter(role == "editor") %>% 
  select(editor, doi, publication.date, manuscript.type, category, measure.names, measure.values) %>% 
  distinct() %>% 
  spread(., key = measure.names, value = measure.values) %>% 
  mutate(total_reads = HTML + PDF)

top_summary <- top_data %>% arrange(desc(total_reads)) %>% 
  select(-HTML, -PDF) %>% head(n = 10)

#10 most frequently cited articles published in the last 1-3 years, ranked on full-text & pdf views

top_cited_data <- data %>% filter_12_to_36_mo(., .$publication.date) %>% 
  filter(journal == this_journal) %>% 
  filter(measure.names == "Total Article Cites") %>% #restrict to cites
  filter(role == "editor") %>% 
  select(editor, doi, title, publication.date, manuscript.type, 
         category, measure.names, measure.values) %>% 
  distinct() %>% select(-measure.names)

#get vector of urls for top 10 read papers
top_cited_url <- top_cited_data %>% 
  arrange(desc(measure.values)) %>% #arrange from highest to lowest reads
  head(n = 10) %>% #select top 10
  mutate(url = paste0("https://www.dx.doi.org/", doi)) %>% #add column with link to paper
  pull(url) #generate vector

#get summary table of top 10 cited papers w. link 
top_cited_summary <- top_cited_data %>% 
  arrange(desc(measure.values)) %>% #arrange from highest to lowest reads
  select(-doi) %>% 
  head(n = 10) %>% #select top 10
  mutate(title = str_to_title(title)) %>% #make all titles in title case
  mutate(title = cell_spec(title, "html", link = top_cited_url)) #embed title w. URL

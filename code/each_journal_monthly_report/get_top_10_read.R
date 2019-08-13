#10 most frequently read articles (& links) published in the last 12 months, ranked on full-text & pdf views

top_data <- data %>% filter_12_mo(., .$publication.date) %>% #articles published in last 12 months
  filter(journal == this_journal) %>% 
  filter(measure.names == "PDF" | measure.names == "HTML") %>% #restrict to full text views
  filter(role == "editor") %>% #single entry
  select(editor, title, doi, publication.date, 
         manuscript.type, category, measure.names, measure.values) %>% 
  distinct() %>% 
  spread(., key = measure.names, value = measure.values) %>% #spread measurements to facilitate addition
  mutate(total_reads = HTML + PDF) #calculate total reads from html & pdf

#get vector of urls for top 10 read papers
top_url <- top_data %>% arrange(desc(total_reads)) %>% #arrange from highest to lowest reads
  select(-HTML, -PDF) %>% head(n = 10) %>% #select top 10
  mutate(url = paste0("https://www.dx.doi.org/", doi)) %>% #add column with link to paper
  pull(url) #vector of urls from top 10

#get summary table of top 10 read papers w. link 
top_summary <- top_data %>% arrange(desc(total_reads)) %>% #arrange from higest to lowest
  select(-HTML, -PDF, -doi) %>% head(n = 10) %>% #select top 10
  mutate(title = str_to_title(title)) %>% #make all titles in title case
  mutate(title = cell_spec(title, "html", link = top_url)) #embed title w. URL

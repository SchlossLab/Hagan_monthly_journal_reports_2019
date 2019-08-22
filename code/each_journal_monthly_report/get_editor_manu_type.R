#**Acceptance and Rejection Rate Report by article type**
#  
#  Research vs transfer vs other
#Data needed: manuscript type, category, decision, editor name

article_data <- data %>% filter(journal == this_journal) %>% 
  filter(year(approved.date) == this_year) %>% #papers submitted this year
  select(manuscript.number, manuscript.type, category, ejp.decision) %>% unique()

#vector of manuscript types
article_list <- article_data %>% pull(manuscript.type) %>% unique()

#summary of submissions by manuscript type
article_totals <- article_data %>% group_by(manuscript.type) %>% 
  summarise(n =n())

#summary table of percent decisions made by manuscript type----
article_summary <- article_data %>% 
  group_by(manuscript.type, ejp.decision) %>% 
  summarise(n = n()) %>% as.data.frame() %>% #number of articles
  left_join(., article_totals, by = "manuscript.type") %>% #join to total manuscript type
  mutate(percent = get_percent(n.x, n.y)) %>% #calculate percent of each decision
  select(-n.y)

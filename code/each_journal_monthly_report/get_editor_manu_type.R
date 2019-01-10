#**Acceptance and Rejection Rate Report by article type**
#  
#  Research vs transfer vs other
#Data needed: manuscript type, category, decision, editor name

article_data <- data %>% filter(journal == this_journal) %>% 
  #mutate(manuscript.type = fct_collapse(manuscript.type, 
                                        #"Minireview" = c("Minireview", "Invited Minireview"),
                                        #"Research Article" = c("Full-length text", "Full-Length Text", "Research Article"),
                                        #"Observation" = c("Short Form", "Short-Form Paper", "Observation"))) %>% 
  #filter(manuscript.type == c("Research Article", "Minireview", "Observation")) %>% 
  filter(year(submitted.date) == this_year) %>% 
  select(manuscript.number, manuscript.type, category, ejp.decision) %>% unique()

article_list <- article_data %>% pull(manuscript.type) %>% unique()

article_totals <- article_data %>% group_by(manuscript.type) %>% 
  summarise(n =n())

article_summary <- article_data %>% 
  group_by(manuscript.type, ejp.decision) %>% 
  summarise(n = n()) %>% as.data.frame() %>% 
  left_join(., article_totals, by = "manuscript.type") %>% 
  mutate(percent = get_percent(n.x, n.y)) %>% 
  select(-n.y)

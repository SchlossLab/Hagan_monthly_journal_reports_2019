#**Acceptance and Rejection Rate Report by article type**
#  
#  Research vs transfer vs other
#Data needed: manuscript type, category, decision, editor name

editor_manu_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  mutate(manuscript.type = fct_collapse(manuscript.type, 
                                        "Minireview" = c("Minireview", "Invited Minireview"),
                                        "Research Article" = c("Full-length text", "Full-Length Text", "Research Article"),
                                        "Observation" = c("Short Form", "Short-Form Paper", "Observation"))) %>% 
  filter(manuscript.type == c("Research Article", "Minireview", "Observation") %>% 
  select(manuscript.number, manuscript.type, category, EJP.decision, editor_fix) %>% unique()

editor_manu_all <- editor_manu_data %>% group_by(editor_fix, manuscript.type) %>% summarise(N = n())

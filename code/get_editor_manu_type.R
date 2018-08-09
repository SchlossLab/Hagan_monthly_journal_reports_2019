#**Acceptance and Rejection Rate Report by article type**
#  
#  Research vs transfer vs other
#Data needed: manuscript type, category, decision, editor name

needed_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  select(manuscript.number, manuscript.type, category, EJP.decision, editor_fix) %>% distinct()

needed_data %>% group_by(editor_fix, manuscript.type) %>% summarise(n = n()) %>% View()

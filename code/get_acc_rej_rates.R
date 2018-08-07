#**Acceptance & rejection rates (YTD?)**
#  
#  Columns: journal, percent accept, reject, and editorial reject
#Row: total
#Data needed: journal, ejp decisions

needed_data <- clean_test_data %>% select(Journal, manuscript.number, EJP.decision) %>% distinct() %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject")

summary <- needed_data %>% group_by(Journal, EJP.decision) %>% summarise(n = n()) %>% View()

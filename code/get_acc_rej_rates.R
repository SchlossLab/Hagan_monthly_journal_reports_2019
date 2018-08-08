#**Acceptance & rejection rates (YTD?)**
#  
#  Columns: journal, percent accept, reject, and editorial reject
#Row: total
#Data needed: journal, ejp decisions

needed_data <- clean_test_data %>% 
  select(Journal, manuscript.number, EJP.decision, version.reviewed) %>% distinct() 

all_desc <- needed_data %>% group_by(Journal) %>% distinct(manuscript.number) %>% 
  summarise(Total = n()) %>% as.data.frame()

ed_reject <- needed_data %>% filter(EJP.decision == "Reject" & is.na(version.reviewed)) %>% 
  group_by(Journal) %>% distinct(manuscript.number) %>% summarise(Editorial_rejections = n()) %>% 
  as.data.frame() %>% 
  mutate(percent_ed_rej = get_percent(Editorial_rejections, all_desc[,2]))

reject <- needed_data %>% filter(EJP.decision == "Reject" & !is.na(version.reviewed)) %>% 
  group_by(Journal) %>% distinct(manuscript.number) %>%
  summarise(Rejected = n()) %>% as.data.frame() %>% 
  mutate(percent_rej = get_percent(Rejected, all_desc[,2]))

accept <- needed_data %>% filter(EJP.decision == "Accept, no revision") %>% 
  group_by(Journal) %>% 
  distinct(manuscript.number) %>% 
  summarise(Accepted = n()) %>% as.data.frame() %>% 
  mutate(percent_accepted = get_percent(Accepted, all_desc[,2]))

summary <- bind_cols(ed_reject, reject[,2:3], accept[,2:3]) #percents aren't adding up for some reason...

needed_data %>% group_by(EJP.decision) %>% summarise(n = n())

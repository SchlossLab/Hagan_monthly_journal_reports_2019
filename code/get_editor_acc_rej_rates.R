#**Acceptance and Rejection Rate Report by editor**
#  
#  Columns: editor, # recommendations, # of accept rec, % of accepts, # of reject rec, % of rejects, # reject w/o #review, % reject w/o review
#Rows: editor, column totals
#(final decision made in ytd)
#Data needed: editor name, decisions

needed_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  select(editor_fix, manuscript.number, EJP.decision, version.reviewed) %>% distinct()

all_desc <- fixed_data %>% group_by(manuscript.number) %>% summarise(n = n()) %>% nrow()

ed_reject <- fixed_data %>% filter(EJP.decision == "Reject" & is.na(version.reviewed)) %>%
  group_by(editor_fix) %>% summarise(Editorial_rejections = n()) %>% 
  as.data.frame() %>% 
  mutate(percent_ed_rej = get_percent(Editorial_rejections, all_desc))

reject <- fixed_data %>% filter(EJP.decision == "Reject" & !is.na(version.reviewed)) %>% 
  group_by(editor_fix) %>% summarise(Rejected = n()) %>% as.data.frame() %>% 
  mutate(percent_rej = get_percent(Rejected, all_desc))

accept <- fixed_data %>% filter(EJP.decision == "Accept, no revision") %>% 
  group_by(editor_fix) %>% summarise(Accepted = n()) %>% as.data.frame() %>% 
  mutate(percent_accepted = get_percent(Accepted, all_desc))

summary <- bind_cols(ed_reject, reject[,2:3], accept[,2:3]) #need to test w. newer data b/c of rej. stats have no editor


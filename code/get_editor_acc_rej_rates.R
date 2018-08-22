#**Acceptance and Rejection Rate Report by editor**
#  
#  Columns: editor, # recommendations, # of accept rec, % of accepts, # of reject rec, % of rejects, # reject w/o #review, % reject w/o review
#Rows: editor, column totals
#(final decision made in ytd)
#Data needed: editor name, decisions

editor_acc_rej_data <- clean_test_data %>% filter(Journal == "mBio") %>% 
  filter(EJP.decision == "Reject"| EJP.decision == "Accept, no revision") %>% 
  select(editor_fix, manuscript.number, EJP.decision, version.reviewed) %>% unique()

editor_all_desc <- editor_acc_rej_data %>% group_by(editor_fix) %>% summarise(N = n())

editor_ed_reject <- editor_acc_rej_data %>% filter(EJP.decision == "Reject" & is.na(version.reviewed)) %>%
  group_by(editor_fix) %>% summarise(Editorial_rejections = n()) %>% as.data.frame() %>% View()
  mutate(percent_ed_rej = get_percent(Editorial_rejections, editor_all_desc$N)) #no rejections assigned to editors for this data

editor_reject <- editor_acc_rej_data %>% filter(EJP.decision == "Reject" & !is.na(version.reviewed)) %>% 
  group_by(editor_fix) %>% summarise(Rejected = n()) %>% as.data.frame() %>% 
  mutate(percent_rej = get_percent(Rejected, editor_all_desc$N)) #no rejections assigned to editors for this dataset

editor_accept <- editor_acc_rej_data %>% filter(EJP.decision == "Accept, no revision") %>% 
  group_by(editor_fix) %>% summarise(Accepted = n()) %>% as.data.frame() %>% 
  mutate(percent_accepted = get_percent(Accepted, editor_all_desc$N))

editor_acc_rej_summary <- bind_cols(editor_ed_reject, editor_reject[,2:3], editor_accept[,2:3]) #need to test w. newer data b/c of rej. stats have no editor


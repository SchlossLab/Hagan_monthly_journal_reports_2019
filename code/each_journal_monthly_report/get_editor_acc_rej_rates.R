#**Acceptance and Rejection Rate Report by editor**
#  
#  Columns: editor, # recommendations, # of accept rec, % of accepts, # of reject rec, % of rejects, # reject w/o #review, % reject w/o review
#Rows: editor, column totals
#(final decision made in ytd)
#Data needed: editor name, decisions

editor_acc_rej_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_mo(., ymd_hms(.$decision.date)) %>% 
  filter(ejp.decision == "Reject"| ejp.decision == "Accept, no revision") %>% 
  select(editor, manuscript.number, ejp.decision, version.reviewed) %>% unique()

editor_all_desc <- editor_acc_rej_data %>% group_by(editor) %>% summarise(n = n())

editor_ed_reject <- editor_acc_rej_data %>% filter(ejp.decision == "Reject" & is.na(version.reviewed)) %>%
  group_by(editor) %>% summarise(editorial_rejections = n()) %>% as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>% 
  mutate(percent_ed_rej = get_percent(editorial_rejections, editor_all_desc$n)) 

editor_reject <- editor_acc_rej_data %>% filter(ejp.decision == "Reject" & !is.na(version.reviewed)) %>% 
  group_by(editor) %>% summarise(rejected = n()) %>% as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>%
  mutate(percent_rej = get_percent(rejected, editor_all_desc$n)) 

editor_accept <- editor_acc_rej_data %>% filter(ejp.decision == "Accept, no revision") %>% 
  group_by(editor) %>% summarise(accepted = n()) %>% as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>%
  mutate(percent_accepted = get_percent(accepted, editor_all_desc$n))

editor_acc_rej_summary <- bind_cols(editor_ed_reject, editor_reject[,2:3], editor_accept[,2:3]) 


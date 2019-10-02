#**Acceptance and Rejection Rate Report by editor**
#  
#  Columns: editor, # recommendations, # of accept rec, % of accepts, # of reject rec, % of rejects, # reject w/o #review, % reject w/o review
#Rows: editor, column totals
#(final decision made in ytd)
#Data needed: editor name, decisions

editor_acc_rej_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_mo(., ymd_hms(.$decision.date)) %>% #decisions made in last 12 months
  filter(ejp.decision == "Reject"| ejp.decision == "Accept, no revision") %>% #restrict to reject/accept
  select(editor, manuscript.number, ejp.decision, reviewed) %>% 
  unique() #single entries only

#count manuscript decisions made by each editor
editor_all_desc <- editor_acc_rej_data %>% 
  group_by(editor) %>% summarise(n = n()) 

#summary of proportion of decisions that were editorial rejections---- 
editor_ed_reject <- editor_acc_rej_data %>% 
  filter(ejp.decision == "Reject" & reviewed == "no") %>% #identify editorial rejections
  group_by(editor) %>% summarise(editorial_rejections = n()) %>% #count per editor
  as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>% #generate df listing all editors
  mutate(percent_ed_rej = get_percent(editorial_rejections, editor_all_desc$n)) #calculate percent

#summary of proportion of decisions that were rejected after review---- 
editor_reject <- editor_acc_rej_data %>% 
  filter(ejp.decision == "Reject" & reviewed == "yes") %>% #identify rejections following review
  group_by(editor) %>% summarise(rejected = n()) %>% #count per editor
  as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>% #generate df listing all editors
  mutate(percent_rej = get_percent(rejected, editor_all_desc$n)) #calculate percent

#summary of proportion of decisions that were accepted ---- 
editor_accept <- editor_acc_rej_data %>% 
  filter(ejp.decision == "Accept, no revision") %>% #identify accepted manuscripts
  group_by(editor) %>% summarise(accepted = n()) %>% #count per editor
  as.data.frame() %>% test_list(editor_all_desc, ., "editor") %>% #generate df listing all editors
  mutate(percent_accepted = get_percent(accepted, editor_all_desc$n)) #calculate percent

#summary table combining accept/reject rates----
editor_acc_rej_summary <- bind_cols(editor_ed_reject, 
                                    editor_reject[,2:3], editor_accept[,2:3]) 


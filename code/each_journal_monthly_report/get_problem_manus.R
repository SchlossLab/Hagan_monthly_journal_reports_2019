#Revised manuscripts without a final decision after 90 days. 
#Column: manuscript number, editor, recieved date, first decision date, days since first decision, days in #folder, days in system, manuscript type, status
#Data needed: what status is problematic?, days in system, days in folder, days since first decision

problem_data <- data %>% 
  filter(journal == this_journal) %>% 
  filter(status == "Waiting for Revision") %>% 
  filter(decision.date >= days(90)) %>% 
  mutate(days.since.decision = as.duration(ymd_hms(decision.date) %--% today())/ddays(1)) %>% 
  select(manuscript.number, editor, approved.date, decision.date, days.since.decision, status)

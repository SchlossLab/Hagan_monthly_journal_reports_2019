#transfer data for each journal

J_transfer_data <- data %>% 
  filter(journal == this_journal) %>% 
  filter(!is.na(transfer.journal) & transfer.type == "From") %>%
  filter(role == "editor") %>% 
  rename(transfer.from = transfer.journal) %>% 
  mutate(transfer.date = date(transfer.date))

#1. how many transfers to each journal in the previous month
J_recent_transfers <- J_transfer_data %>% 
  filter(transfer.date %within% interval(start = (today() - days(30)), end = today())) %>% 
  select(transfer.date, transfer.from, manuscript.number, title, editor, ejp.decision)
  
#2. what percentage of transfers accepted in the previous year
J_transfers_ytd <- J_transfer_data %>% 
  filter_12_mo(., .$transfer.date) %>% 
  filter(ejp.decision == "Reject" | ejp.decision == "Accept, no revision") %>% 
  group_by(ejp.decision) %>% summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

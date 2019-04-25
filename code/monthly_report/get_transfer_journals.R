
all_transfer_data <- data %>% 
  filter(!is.na(transfer.journal) & transfer.type == "From") %>% 
  filter(role == "editor") %>% 
  rename(transfer.from = transfer.journal) %>% 
  mutate(transfer.date = date(transfer.date)) %>% 
  filter(year(submitted.date) == this_year) 

#Where are transfers coming from 
all_trans_from <- all_transfer_data %>% 
  select(transfer.from, manuscript.number) %>% distinct() %>% 
  group_by(transfer.from) %>% summarise(n = n()) %>% arrange(desc(n)) %>% 
  mutate(journal = transfer.from,
         transfer = "From") %>% 
  select(-transfer.from)

#Where are transfers going to?
all_trans_to <- all_transfer_data %>% 
  select(journal, manuscript.number) %>% distinct() %>% 
  group_by(journal) %>% summarise(n = n()) %>% arrange(desc(n)) %>% 
  mutate(transfer = "To")

all_trans <- rbind(all_trans_from, all_trans_to)

plot_trans <- all_trans %>% 
  ggplot()+
  geom_col(aes(x = journal, y = n, fill = transfer))+
  facet_wrap(~transfer)+
  labs(x = "Journal", y = "Number of Transfers", 
       caption = "All transfers occuring in this calendar year")+
  my_theme_horiz

#What percentage of transfers accepted in the previous year
all_transfers_ytd <- all_transfer_data %>% 
  filter(EJP.decision == "Reject" | EJP.decision == "Accept, no revision") %>% 
  group_by(EJP.decision) %>% summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

#statistics on transfers conducted YTD

#data needed: journals transferred to/from, date

#select data
all_transfer_data <- data %>% 
  filter(!is.na(transfer.journal) & transfer.type == "From") %>% #eliminate non-transferred manuscripts & a single transfer type (to avoid duplicates)
  filter(role == "editor") %>% #single entry per manuscript
  rename(transfer.from = transfer.journal) %>% 
  mutate(transfer.date = date(transfer.date)) %>% #convert to manipulatable date
  filter(year(submitted.date) == this_year) #restrict to calendar year

#Where are transfers coming from 
all_trans_from <- all_transfer_data %>% 
  select(transfer.from, manuscript.number) %>% distinct() %>% 
  group_by(transfer.from) %>% summarise(n = n()) %>% #count transfers from journals
  arrange(desc(n)) %>% #highest to lowest
  mutate(journal = transfer.from,
         transfer = "From") %>% 
  select(-transfer.from)

#Where are transfers going to?
all_trans_to <- all_transfer_data %>% 
  select(journal, manuscript.number) %>% distinct() %>% 
  group_by(journal) %>% summarise(n = n()) %>% #count transfers to journals
  arrange(desc(n)) %>% #highest to lowest
  mutate(transfer = "To") 

all_trans <- rbind(all_trans_from, all_trans_to) #combine & tidy data

plot_trans <- all_trans %>% 
  ggplot()+
  geom_col(aes(x = journal, y = n, fill = transfer))+
  facet_wrap(~transfer)+
  coord_flip()+
  labs(x = "Journal", y = "Number of Transfers", 
       caption = "All transfers occuring in this calendar year")+
  my_theme_horiz

#What percentage of transfers accepted in the previous year
all_transfers_ytd <- all_transfer_data %>% 
  filter(ejp.decision == "Reject" | ejp.decision == "Accept, no revision") %>% 
  group_by(ejp.decision) %>% summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

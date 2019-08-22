#**Submissions Table**
#  
#Columns: journal, each month YTD
#Row: total (also plotted)
#Data needed: journal, submission date


subs_data <- data %>% 
  filter(ejp.decision != "Withdrawn") %>% #remove any submissions withdrawn by the author/rejected for scope
  select(approved.date, journal, manuscript.number, version) %>% #pull needed data (uses author approved date instead of sub.date)
  filter(version == "0") %>% distinct() %>% #restrict to inital submission
  filter(year(approved.date) == this_year) %>% #only manuscripts submitted this year
  mutate(sub.month = month(approved.date, label = TRUE)) #recode months into abbrevations

#get monthly totals for each journal
subs_by_journ <- subs_data %>% group_by(journal, sub.month) %>% 
  summarise(n = n()) %>% #count subs for ea month by journal
  filter(journal != "NA") %>% #drop counts unassigned to a journal
  spread(., key = sub.month, value = n) %>% as.data.frame() #move months to columns along with journals

#get monthly totals across all journals
journal <- "Total" #new row for totals

sum_month <- subs_data %>% group_by(sub.month) %>% summarize(n = n()) %>% #count subs for ea month
  spread(sub.month, n) %>% as.data.frame() %>% #transform to single row
  cbind(journal, .) %>% #add column with "total"
  rbind(subs_by_journ, .) #add totals row to the end of the summary table

sum_month <- rename(sum_month, "Journal" = "journal")

#**Submissions**
#  
#  Columns: journal, each month YTD
#Row: total (also plotted)
#Data needed: journal, submission date


subs_data <- data %>% filter(ejp.decision != "Withdrawn") %>% #remove any submissions withdrawn by the author/rejected for scope
  select(approved.date, journal, manuscript.number, version) %>% #pull needed data (uses author approved date instead of sub.date)
  filter(version == "0") %>% distinct() %>% #restrict to inital submission
  filter(year(approved.date) == this_year) %>% 
  mutate(sub.month = month(approved.date, label = TRUE)) #recode months into abbrevations

subs_by_journ <- subs_data %>% group_by(journal, sub.month) %>% summarise(n = n()) %>% #count subs for ea month by journal
  filter(journal != "NA") %>% #drop counts unassigned to an article
  spread(., key = sub.month, value = n) %>% as.data.frame() #move months to columns along with journals

journal <- "Total" #new row for totals

sum_month <- subs_data %>% group_by(sub.month) %>% summarize(n = n()) %>% #count subs by month
  spread(sub.month, n) %>% as.data.frame() %>% cbind(journal, .) %>% #move into single row & add column with "total"
  rbind(subs_by_journ, .) #add totals row to rest of summary table

sum_month <- rename(sum_month, "Journal" = "journal")

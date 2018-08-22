#**Submissions**
#  
#  Columns: journal, each month YTD
#Row: total (also plotted)
#Data needed: journal, submission date

subs_data <- clean_test_data %>% select(submitted.date, Journal, manuscript.number, version) %>% #pull needed data
  filter(version == "0") %>% distinct() %>% #restrict to inital submission
  separate(submitted.date, "submitted.date", sep = " ", extra = "drop") %>% #drop time from submitted date
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% #split submitted month out
  mutate(month = fct_recode(month, "Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", "May" = "05",
                            "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10",
                            "Nov" = "11", "Dec" = "12")) #recode months into abbrevations

subs_by_journ <- subs_data %>% group_by(Journal, month) %>% summarise(n = n()) %>% #count subs for ea month by journal
  filter(Journal != "NA") %>% #drop counts unassigned to an article
  spread(., key = month, value = n) %>% as.data.frame() #move months to columns along with journals

Journal <- "Total" #new row for totals

sum_month <- subs_data %>% group_by(month) %>% summarize(n = n()) %>% #count subs by month
  spread(month, n) %>% as.data.frame() %>% cbind(Journal, .) %>% #move into single row & add column with "total"
  rbind(subs_by_journ, .) #add totals row to rest of summary table

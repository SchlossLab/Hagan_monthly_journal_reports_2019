#**Submissions**
#  
#  Columns: journal, each month YTD
#Row: total (also plotted)
#Data needed: journal, submission date

needed_data <- clean_test_data %>% select(submitted.date, Journal, manuscript.number, version) %>% 
  filter(version == "0") %>% distinct() %>% 
  separate(submitted.date, "submitted.date", sep = " ", extra = "drop") %>% 
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% 
  mutate(month = fct_recode(month, "Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", "May" = "05",
                            "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10",
                            "Nov" = "11", "Dec" = "12"))

subs_by_journ <- needed_data %>% group_by(Journal, month) %>% summarise(n = n()) %>% 
  filter(Journal != "NA") %>% 
  spread(., key = month, value = n) %>% as.data.frame()

Journal <- "Total"

sum_month <- needed_data %>% group_by(month) %>% summarize(n = n()) %>% 
  spread(month, n) %>% as.data.frame() %>% cbind(Journal, .) %>% rbind(subs_by_journ, .)

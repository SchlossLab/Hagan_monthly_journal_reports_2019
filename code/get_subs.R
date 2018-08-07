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

summarized <- needed_data %>% group_by(Journal, month) %>% summarise(n = n()) %>% 
  filter(Journal != "NA") %>% 
  spread(., key = month, value = n) %>% 
  cbind(.rowSums(2, 13))

summary_row <- data.frame("All", colSums(summarized[,2:13])) %>% 
  spread(., key = , value = [,2])
  mutate(YTD = rowSums(.[,2:13]))

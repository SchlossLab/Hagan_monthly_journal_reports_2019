#Difference in submissions from previous year
#Columns: journal, 2014 - 2017, 2018 (ytd) 
#Final rows: all journals, all journals except mSystems & mSphere
#Data needed: journal, submission date - everything from last 4-5 years

needed_data <- clean_test_data %>% 
  filter(version == "0") %>% 
  filter(Journal != "NA") %>% 
  select(submitted.date, Journal, manuscript.number) %>% 
  separate(submitted.date, "submitted.date", sep = " ", extra = "drop") %>% 
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% distinct()

journ_by_year <- needed_data %>% group_by(Journal, year) %>% summarize(n = n()) %>% 
  spread(year, n) %>% as.data.frame()

Journal <- "Total"

add_sums <- needed_data %>% group_by(year) %>% summarise(n = n()) %>% spread(year, n) %>% as.data.frame() %>% cbind(Journal, .) %>% rbind(journ_by_year, .)

diff_from_prev_year <- round((((add_sums[,ncol(add_sums)]-add_sums[,ncol(add_sums)-1])/add_sums[,ncol(add_sums)-1])*100), digits = 2)

all_summary <- cbind(add_sums, diff_from_prev_year)

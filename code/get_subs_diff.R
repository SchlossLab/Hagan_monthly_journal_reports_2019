#Difference in submissions from previous year
#Columns: journal, 2014 - 2017, 2018 (ytd) 
#Final rows: all journals, all journals except mSystems & mSphere
#Data needed: journal, submission date - everything from last 4-5 years

needed_data <- clean_test_data %>% 
  filter(version == "0") %>% 
  select(submitted.date, Journal, manuscript.number) %>% 
  separate(submitted.date, "submitted.date", sep = " ", extra = "drop") %>% 
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% distinct()

summary <- needed_data %>% group_by(Journal, year) %>% summarize(n = n()) %>% 
  spread(year, n)

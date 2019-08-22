#Difference in submissions from previous year -- not included in report
#Columns: journal, 2014 - 2017, 2018 (ytd) 
#Final rows: all journals, all journals except mSystems & mSphere
#Data needed: journal, submission date - everything from last 4-5 years

subs_dif_data <- data %>% 
  filter(ejp.decision != "Withdrawn") %>% 
  filter(version == "0") %>% filter(journal != "NA") %>% #restrict to inital subs & drop journals
  select(approved.date, journal, manuscript.number) %>% #pull necessary columns
  separate(approved, "submitted.date", sep = " ", extra = "drop") %>% #drop time from sub date
  separate(approved, c("year", "month", "day"), sep = "-") %>% distinct() #separate month/yr submitted

journ_by_year <- subs_dif_data %>% group_by(journal, year) %>% summarize(n = n()) %>% #count subs by journal per year
  spread(year, n) %>% as.data.frame() #break years into columns

journal <- "Total" #start row for yearly totals

add_sums <- subs_dif_data %>% group_by(year) %>% summarise(n = n()) %>% #count yearly totals
  spread(year, n) %>% as.data.frame() %>% #split into single row
  cbind(journal, .) %>% rbind(journ_by_year, .) #add yearly totals to journal

percent.difference <- round((((add_sums[,ncol(add_sums)]-add_sums[,ncol(add_sums)-1])/add_sums[,ncol(add_sums)-1])*100), digits = 2) #calculate difference between previous year and current year

subs_dif_summary <- cbind(add_sums, percent.difference) #combine dfs

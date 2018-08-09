#**Number of original manuscripts assigned to editors**  BY JOURNAL
#  
#  Columns: editor, each month YTD, total, average, & percent of total (ytd). Final two rows are total (for #month across all editors), and cumulative total (all editors, ytd) (includes all article types & withdrawn#/deleted/transferred)
#Data needed: full editor name, journal, date of submission, date assigned to editor, current status (withdrawn#/deleted/transferred), decision

AEM_needed_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  select(manuscript.number, Editor, is.resubmission, related.manu, version, submitted.date) %>% 
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% select(-day)

total <- AEM_needed_data %>% group_by(Editor, month) %>% summarise(n = n()) %>% 
  spread(month, n)

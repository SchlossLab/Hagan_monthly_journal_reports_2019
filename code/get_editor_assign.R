#**Number of original manuscripts assigned to editors**  BY JOURNAL
#  
#  Columns: editor, each month YTD, total, average, & percent of total (ytd). Final two rows are total (for #month across all editors), and cumulative total (all editors, ytd) (includes all article types & withdrawn#/deleted/transferred)
#Data needed: full editor name, journal, date of submission, date assigned to editor, current status (withdrawn#/deleted/transferred), decision

editor_assign_needed_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  select(manuscript.number, editor_fix, is.resubmission, related.manu, version, submitted.date) %>% 
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% select(-day) %>% 
  filter(version == "0") %>% filter(year == "2016")

by_editor <- editor_assign_needed_data %>% 
  group_by(editor_fix, month) %>% summarise(N = n()) %>% 
  spread(month, N) %>% as.data.frame()

month_sums <- by_editor %>% select(-editor_fix) %>% colSums(., na.rm = TRUE) %>% as.data.frame() %>% 
  t() %>% cbind(editor_fix = "Total for month", .) %>% rbind(by_editor, .)

ed_sums <- month_sums %>% select(-editor_fix) %>% sapply(., as.numeric) %>% rowSums(., na.rm = TRUE) %>% as.data.frame()

ed_avg <- month_sums %>% select(-editor_fix) %>% sapply(., as.numeric) %>% rowMeans(., na.rm = TRUE) %>% 
  round(., digits = 2) %>% as.data.frame()

editor_assign_summary <- bind_cols(month_sums, ed_sums, ed_avg) 
names(editor_assign_summary) <- c("Editor", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                      "Oct", "Nov", "Dec", "Total_YTD", "Average")

editor_assign_all_summary <- mutate(editor_assign_summary, 
                                    Percent_YTD = get_percent(Total_YTD, editor_assign_summary[nrow(editor_assign_summary),ncol(editor_assign_summary)-1]))

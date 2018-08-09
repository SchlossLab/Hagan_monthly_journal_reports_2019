#**Time to first decision by editor in days (excluding editorial rejections)**
#  
#  Columns: editor, average, median, low, high, number of manuscripts. Final row: total
#(decision made in ytd)
#Data needed: full editor name, journal, decision, submission date, decision date

needed_data <- clean_test_data %>% filter(Journal == "AEM") %>% 
  select(editor_fix, EJP.decision, version, days.to.decision, review.recommendation, manuscript.number) %>% 
  filter(version == "0" & !is.na(review.recommendation)) %>% distinct()

summary <- needed_data %>% group_by(editor_fix) %>% 
  summarise(n = n(), Avg_days = round(mean(days.to.decision), digits = 2),
                                 Med_days = round(median(days.to.decision), digits = 2), 
                                 Min_days = round(min(days.to.decision), digits = 2),
                                 Max_days = round(max(days.to.decision), digits = 2))

year_sums <- needed_data %>% summarise(n = n(), Avg_days = round(mean(days.to.decision), digits = 2),
                                       Med_days = round(median(days.to.decision), digits = 2), 
                                       Min_days = round(min(days.to.decision), digits = 2),
                                       Max_days = round(max(days.to.decision), digits = 2)) %>% 
  as.data.frame() %>% cbind(editor_fix = "All manuscripts", .) %>% rbind(summary, .)

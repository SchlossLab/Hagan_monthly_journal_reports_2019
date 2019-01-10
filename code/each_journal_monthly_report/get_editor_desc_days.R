#**Time to first decision by editor in days (excluding editorial rejections)**
#  
#  Columns: editor, average, median, low, high, number of manuscripts. Final row: total
#(decision made in ytd)
#Data needed: full editor name, journal, decision, submission date, decision date

editor_desc_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_mo(., ymd_hms(.$decision.date)) %>% 
  select(editor, ejp.decision, version, days.to.decision, review.recommendation, manuscript.number) %>% 
  filter(version == "0" & !is.na(review.recommendation)) %>% distinct()

editor_desc_summary <- editor_desc_data %>% group_by(editor) %>% 
  summarise(n = n(), Avg_days = round(mean(days.to.decision), digits = 2),
                                 Med_days = round(median(days.to.decision), digits = 2), 
                                 Min_days = round(min(days.to.decision), digits = 2),
                                 Max_days = round(max(days.to.decision), digits = 2))

editor_desc_year_sums <- editor_desc_data %>% summarise(n = n(), Avg_days = round(mean(days.to.decision), digits = 2),
                                       Med_days = round(median(days.to.decision), digits = 2), 
                                       Min_days = round(min(days.to.decision), digits = 2),
                                       Max_days = round(max(days.to.decision), digits = 2)) %>% 
  as.data.frame() %>% cbind(editor = "All manuscripts", .) %>% rbind(editor_desc_summary, .)

colnames(editor_desc_year_sums)[1] <- "Editor"

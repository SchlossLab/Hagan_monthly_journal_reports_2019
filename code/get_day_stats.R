#Time to first decision/publication (Days)
#  
#Columns: journal, average & median to first decision, average time to publication (check timespan)
#Row: total
#Data needed: submission date, decision date, publication date, journal title#

#first decision excludes editorial rejections

needed_data <- clean_test_data %>% 
  select(Journal, submitted.date, days.to.decision, `Date of Publication`, 
         EJP.decision, manuscript.number, version, version.reviewed) %>%
  separate(submitted.date, c("date.submitted", "c"), sep = " ", extra = "drop") %>% 
  mutate(days.to.publication = `Date of Publication` - ymd(date.submitted)) %>% 
  filter(!is.na(version.reviewed) & EJP.decision != "Reject")

summary_1 <- needed_data %>% filter(version == "0") %>% group_by(Journal) %>% 
  summarise(n.first.decision = n(), avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), med.first.days.to.decision = median(days.to.decision, na.rm = TRUE))

summary_2 <- needed_data %>% filter(!is.na(`Date of Publication`)) %>% group_by(Journal) %>% 
  summarise(n.pub = n(), avg.days.to.pub = mean(days.to.publication, na.rm = TRUE))

summary_all_1 <- needed_data %>% filter(version == "0") %>%
  summarise(n.first.decision = n(), avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), med.first.days.to.decision = median(days.to.decision, na.rm = TRUE))

summary_all_2 <- needed_data %>% filter(!is.na(`Date of Publication`)) %>% 
  summarise(n.pub = n(), avg.days.to.pub = mean(days.to.publication, na.rm = TRUE))

summary_all <- data.frame("Total", summary_all_1, summary_all_2)

names(summary_all) <- c("Journal", "n.first.decision", "avg.first.days.to.decision", "med.first.days.to.decision", "n.pub", "avg.days.to.pub")

day_summary <- left_join(summary_1, summary_2, by = "Journal") %>%
  rbind(summary_all) %>% 
  filter(Journal != "NA") %>% 
  mutate(avg.days.to.pub = round(avg.days.to.pub, digits = 2)) %>% 
  mutate(avg.first.days.to.decision = round(avg.first.days.to.decision, digits = 2)) %>% 
  mutate(med.first.days.to.decision = round(med.first.days.to.decision, digits = 2))

  

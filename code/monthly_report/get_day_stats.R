#Time to first decision/publication (Days)
#  
#Columns: journal, average & median to first decision, average time to publication (check timespan)
#Row: total
#Data needed: submission date, decision date, publication date, journal title#

#first decision excludes editorial rejections

needed_data <- data %>% 
  select(journal, approved.date, days.to.decision, publication.date, 
         ejp.decision, manuscript.number, version, version.reviewed) %>%
  separate(approved.date, c("date.submitted", "c"), sep = " ", extra = "drop") %>% #drop hms from submitted date
  mutate(days.to.publication = publication.date - ymd(date.submitted)) %>%#calculate days from submission to pub 
  filter(!is.na(version.reviewed) & ejp.decision != "Reject") %>% #exclude editorial rejections
  filter_12_mo(., ymd(.$date.submitted)) #restrict to manuscripts submitted within the last 12 months
  
#calculate median & mean days to first decision
summary_1 <- needed_data %>% filter(version == "0") %>% #get first decision
  group_by(journal) %>% 
  summarise(n.first.decision = n(), 
            avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), 
            med.first.days.to.decision = median(days.to.decision, na.rm = TRUE))

#table of total published & average days to publication
summary_2 <- needed_data %>% filter(!is.na(publication.date)) %>% #get published articles
  group_by(journal) %>% 
  summarise(n.pub = n(), 
            avg.days.to.pub = mean(days.to.publication, na.rm = TRUE))

summary_all_1 <- needed_data %>% filter(version == "0") %>%
  summarise(n.first.decision = n(), avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), med.first.days.to.decision = median(days.to.decision, na.rm = TRUE))

summary_all_2 <- needed_data %>% filter(!is.na(publication.date)) %>% 
  summarise(n.pub = n(), avg.days.to.pub = mean(days.to.publication, na.rm = TRUE))

summary_all <- data.frame("Total", summary_all_1, summary_all_2)

names(summary_all) <- c("journal", "n.first.decision", "avg.first.days.to.decision", "med.first.days.to.decision", "n.pub", "avg.days.to.pub")

day_summary <- left_join(summary_1, summary_2, by = "journal") %>%
  rbind(summary_all) %>% 
  filter(journal != "NA") %>% 
  mutate(avg.days.to.pub = round(avg.days.to.pub, digits = 2)) %>% 
  mutate(avg.first.days.to.decision = round(avg.first.days.to.decision, digits = 2)) %>% 
  mutate(med.first.days.to.decision = round(med.first.days.to.decision, digits = 2))

  

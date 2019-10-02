#Time to first decision/publication (Days) -- excluding editorial rejections
 
#Columns: journal, average & median to first decision, average time to publication (check timespan)
#Row: total
#Data needed: submission date, decision date, publication date, journal title#

needed_data <- data %>% 
  select(journal, approved.date, days.to.decision, publication.date, 
         ejp.decision, manuscript.number, version, reviewed) %>%
  separate(approved.date, c("date.submitted", "c"), sep = " ", extra = "drop") %>% #drop hms from submitted date
  mutate(days.to.publication = publication.date - ymd(date.submitted)) %>% #calculate days from submission to pub 
  filter(reviewed == "no" & ejp.decision != "Reject") %>% #exclude editorial rejections
  filter_12_mo(., ymd(.$date.submitted)) #restrict to manuscripts submitted within the last 12 months

#for each journal----  
#calculate median & mean days to first decision
summary_1 <- needed_data %>% filter(version == "0") %>% #get first decision
  select(journal, manuscript.number, days.to.decision) %>% distinct() %>% 
  group_by(journal) %>% 
  summarise(n.first.decision = n(), #number of manuscripts
            avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), #average
            med.first.days.to.decision = median(days.to.decision, na.rm = TRUE)) #median

#table of average days to publication
summary_2 <- needed_data %>% filter(!is.na(publication.date)) %>% #get published articles
  select(journal, manuscript.number, days.to.publication) %>% distinct() %>% 
  group_by(journal) %>% 
  summarise(n.pub = n(), #number of publications
            avg.days.to.pub = mean(days.to.publication, na.rm = TRUE)) #average days to publication

#for all journals combined----
#table of median/mean days to first decision
summary_all_1 <- needed_data %>% filter(version == "0") %>% 
  select(manuscript.number, days.to.decision) %>% distinct() %>% 
  summarise(n.first.decision = n(), #number of manuscripts
            avg.first.days.to.decision = mean(days.to.decision, na.rm = TRUE), 
            med.first.days.to.decision = median(days.to.decision, na.rm = TRUE))

#table of average days to publication
summary_all_2 <- needed_data %>% filter(!is.na(publication.date)) %>% 
  select(manuscript.number, days.to.publication) %>% distinct() %>% 
  summarise(n.pub = n(), #number of publications
            avg.days.to.pub = mean(days.to.publication, na.rm = TRUE))

#merge all journals combined data into single data row
summary_all <- data.frame("Total", summary_all_1, summary_all_2)

names(summary_all) <- c("journal", "n.first.decision", "avg.first.days.to.decision", "med.first.days.to.decision", "n.pub", "avg.days.to.pub")

#final summary table----
day_summary <- left_join(summary_1, summary_2, by = "journal") %>% #join each journal data
  rbind(summary_all) %>% #add summary row
  filter(journal != "NA") %>% #drop data for manuscripts unassigned to journals
  mutate(avg.days.to.pub = round(avg.days.to.pub, digits = 2), #round days to two digits
         avg.first.days.to.decision = round(avg.first.days.to.decision, digits = 2),
         med.first.days.to.decision = round(med.first.days.to.decision, digits = 2))

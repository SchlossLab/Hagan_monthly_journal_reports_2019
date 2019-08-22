#estimated jif

jif_report_data <- read_csv(paste0("../processed_data/jif_report_data", this_ym, ".csv")) %>% 
  filter(doi != "na")

calc_jif_data <- jif_report_data %>% 
  mutate(citation.date = mdy(`Citation Date`), #convert dates to be manipulatable
         publication.date = mdy(`Article Date of Publication (article_metadata)`)) %>% 
  filter(publication.date %within% interval(start = (today() - months(48)), 
                                            end = today() - months(24))) %>% #papers published in previous 24 - 48 months
  filter(citation.date %within% interval(start = (today() - months(24)), 
                                            end = today() - months(12))) #citations from previous 12 - 24 months
  
#calc total cites from previous year for each journal----    
total_cites <- calc_jif_data %>% group_by(journal) %>% 
  summarise(total.cites = sum(Cites)) 

#calc number of citeable items----
citable_items <- c("AAM Contribution-Observation", "AAM Contribution-Research Article", 
                   "Author Reply", "Case Report", "Challenging Clinical Case", 
                   "Clinical Microbiology Best Practices", "Commentary", "CVInsights", 
                   "Editorial", "Full-length text", "Full-Length Text", "Invited Commentary", 
                   "Invited Gem", "Invited Minireview", "Methods and Protocols", "Minireview", 
                   "New-Data Letter", "Observation", "Opinion/Hypothesis", "Perspective", 
                   "Point-Counterpoint", "Practical Guidance for Clinical Microbiology",
                   "Research Article", "Resource Report", "Review", "Short Form", "Short-Form Paper")

citable_count <- calc_jif_data %>% 
  filter(manuscript.type %in% citable_items) %>% #select for citable items
  select(journal, doi) %>% distinct() %>% 
  group_by(journal) %>% summarise(citable.count = n()) #count citable items by journal

#table of estimated jifs----
est_jif <- left_join(total_cites, citable_count, by = "journal") %>% #join total cites & countable cites
  filter(!is.na(citable.count)) %>% #eliminate journals w/o citable articles
  mutate(est.jif = round(total.cites/citable.count, digits = 2)) #calcuate jif

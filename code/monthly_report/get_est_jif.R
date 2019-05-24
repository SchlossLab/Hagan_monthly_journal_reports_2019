#estimated jif

jif_report_data <- read_csv(paste0("../processed_data/jif_report_data", this_ym, ".csv")) %>% 
  filter(doi != "na")

calc_jif_data <- jif_report_data %>% 
  #filter(journal == this_journal) %>% #specify journal
  mutate(citation.date = mdy(`Citation Date`),
         publication.date = mdy(`Article Date of Publication (article_metadata)`)) %>% 
  filter(publication.date %within% interval(start = (today() - months(48)), end = today() - months(24))) %>% #filter for papers published in previous 24 - 48 months
  filter(citation.date %within% interval(start = (today() - months(24)), 
                                            end = today() - months(12))) #filter for citations made previous 12 - 24 months
  
#calc total cites in previous year    
total_cites <- calc_jif_data %>% group_by(journal) %>% summarise(total.cites = sum(Cites)) 

  
#calc total citeable items
citable_items <- c("AAM Contribution-Observation", "AAM Contribution-Research Article", "Author Reply", "Case Report", 
                   "Challenging Clinical Case", "Clinical Microbiology Best Practices", "Commentary", "CVInsights", 
                   "Editorial", "Full-length text", "Full-Length Text", "Invited Commentary", "Invited Gem", 
                   "Invited Minireview", "Methods and Protocols", "Minireview", "New-Data Letter", "Observation", 
                   "Opinion/Hypothesis", "Perspective", "Point-Counterpoint", "Practical Guidance for Clinical Microbiology",
                   "Research Article", "Resource Report", "Review", "Short Form", "Short-Form Paper")

citable_count <- calc_jif_data %>% filter(manuscript.type %in% citable_items) %>% 
  select(journal, doi) %>% distinct() %>% 
  group_by(journal) %>% summarise(citable.count = n())

est_jif <- left_join(total_cites, citable_count, by = "journal") %>% 
  filter(!is.na(citable.count)) %>% 
  mutate(est.jif = round(total.cites/citable.count, digits = 2))

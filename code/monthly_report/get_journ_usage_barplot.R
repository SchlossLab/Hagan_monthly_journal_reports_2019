#Plots of usage stats for journal: YTD bar graphs w. web, PDF, unique users

drop.measure.names <- c("Measure By", "Article Cites / Month", 
                        "Published Months", "Total Article Cites")

stats_data <- data %>% 
  filter_12_mo(., .$publication.date) %>% #published in last 12 months
  filter(!(measure.names %in% drop.measure.names)) %>% #select for abstract/html/pdf data
  mutate(measure.values.per.month = measure.values/months.published) #normalize measures by months published

#Abstract stats----
#count # of manuscripts with abstract views above 4000
missed_abstracts <- stats_data %>% filter(measure.names == "Abstract") %>% 
  filter(measure.values.per.month >= 4000) %>% count(manuscript.number) %>% nrow()

abstract_plot <- stats_data %>% filter(measure.names == "Abstract") %>%
    ggplot()+
    geom_boxplot(aes(x = journal, y = measure.values.per.month))+
    labs(title = "Abstract Views", 
         x = "Journal", 
         y = "Abstract Views/# Months Published",
         caption = paste("\nNotes: Restricted to articles published in the last 12 months,\n", 
                         missed_abstracts, "articles recieved greater than 4000 views per month published"))+
  my_theme

#HTML stats----
#count # of manuscripts with html views above 750
missed_html <- stats_data %>% filter(measure.names == "HTML") %>% 
  filter(measure.values.per.month >= 750) %>% count(manuscript.number) %>% nrow()

html_plot <- stats_data %>% filter(measure.names == "HTML") %>%
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values.per.month))+
  labs(title = "HTML Views", 
       x = "Journal", 
       y = "HTML Views/# Months Published",
       caption = paste("\nNotes: Restricted to articles published in the last 12 months, y-axis capped at 1000,\n",
                        missed_html, "articles recieved greater than 750 views per month pubished"))+
  coord_cartesian(ylim = c(0, 750))+
  my_theme

#PDF stats----
#count # of manuscripts with PDF downloads above 300
missed_pdf <- stats_data %>% filter(measure.names == "PDF") %>% 
  filter(measure.values.per.month >= 300) %>% count(manuscript.number) %>% nrow()

pdf_plot <- stats_data %>% filter(measure.names == "PDF") %>%
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values.per.month))+
  labs(title = "PDF Views", 
       x = "Journal", 
       y = "PDF Views/# Months Published",
       caption = paste("\nNotes: Restricted to articles published in the last 12 months, y-axis capped at 300,\n",
                       missed_pdf, "articles recieved greater than 300 views per month pubished"))+
  coord_cartesian(ylim = c(0, 300))+
  my_theme


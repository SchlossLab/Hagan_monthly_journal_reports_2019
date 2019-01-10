#Plots of usage stats for journal: YTD bar graphs w. web, PDF, unique users

drop.measure.names <- c("Measure By", "Article Cites / Month", "Published Months", "Total Article Cites")

stats_data <- data %>% 
  filter_12_mo(., .$publication.date) %>% #published in last 12 months
  filter(!(measure.names %in% drop.measure.names)) %>% 
  mutate(measure.values.per.month = measure.values/months.published) 

missed_abstracts <- stats_data %>% filter(measure.names == "Abstract") %>% 
  filter(measure.values.per.month >= 4000) %>% count(manuscript.number) %>% nrow()

abstract_plot <- stats_data %>% filter(measure.names == "Abstract") %>%
    ggplot()+
    geom_boxplot(aes(x = journal, y = measure.values.per.month))+
    labs(title = "Abstract Views", 
         x = "Journal", 
         y = "Abstract Views/# Months Published",
         caption = paste("Notes: Views of articles published in the last 12 months, y-axis restricted to 4000,", missed_abstracts, "articles recieved greater than 4000 views per month the articles were pubished"))+
  coord_cartesian(ylim = c(0, 4000))+
  my_theme

missed_html <- stats_data %>% filter(measure.names == "HTML") %>% 
  filter(measure.values.per.month >= 750) %>% count(manuscript.number) %>% nrow()

html_plot <- stats_data %>% filter(measure.names == "HTML") %>%
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values.per.month))+
  labs(title = "HTML Views", 
       x = "Journal", 
       y = "HTML Views/# Months Published",
       caption = paste("Notes: Views of articles published in the last 12 months, y-axis restricted to 1000,", missed_html, "articles recieved greater than 1000 views per month the articles were pubished"))+
  coord_cartesian(ylim = c(0, 750))+
  my_theme

missed_pdf <- stats_data %>% filter(measure.names == "PDF") %>% 
  filter(measure.values.per.month >= 300) %>% count(manuscript.number) %>% nrow()

pdf_plot <- stats_data %>% filter(measure.names == "PDF") %>%
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values.per.month))+
  labs(title = "PDF Views", 
       x = "Journal", 
       y = "PDF Views/# Months Published",
       caption = paste("Notes: Views of articles published in the last 12 months, y-axis restricted to 300,", missed_pdf, "articles recieved greater than 300 views per month the articles were pubished"))+
  coord_cartesian(ylim = c(0, 300))+
  my_theme
#probably going to have to loop through the various measures to generate independent summaries for each journal that can be bound in to a df to be used for a barplot


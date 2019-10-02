#Plots of usage stats for journal: YTD bar graphs w. web, PDF, unique users

#dataset
journ_usage_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_mo(., .$publication.date) %>% #usage stats from last year
  select(measure.names, measure.values, category, publication.date, 
         manuscript.type) %>% distinct() %>% 
  filter(measure.names == "Abstract" | measure.names == "HTML" | measure.names == "PDF") %>% 
  mutate(measure.values.per.k = measure.values/100) %>% #actually values per 100, not 1000(k)
  mutate(category = strtrim(category, 45)) #restrict category length to 45 characters

#usage stats plotted according measure type
total_views <- journ_usage_data %>% 
  group_by(measure.names) %>% 
  summarise(total = sum(measure.values.per.k, na.rm = TRUE)) %>% #add totals for each measure
  ggplot()+
  geom_col(aes(x = measure.names, y = total))+
  labs(title = "Total Article Views", 
       subtitle = "Includes articles published in last 12 months", x = "Metric", 
       y = "Total Views (x100)")+
  my_theme_horiz

#usage stats plotted according to month articles were published
date_views <- journ_usage_data %>% 
  group_by(floor_date(ymd(publication.date), 
                      unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values.per.k, na.rm = TRUE)) %>% #usage by month published
  ggplot()+
  geom_line(aes(x = `floor_date(ymd(publication.date), unit = "month")`, 
                y = total.views, color = measure.names))+
  facet_wrap(~ measure.names, scales = "free_y", ncol = 1)+
  labs(y = "Total Views (x100)", x = "Month of Publication", 
       title = "Total Views of Articles by Metric",
       subtitle = "Includes articles published in last 12 months")+
  my_theme_horiz

#usage stats plotted according to manuscript category/type
cat_views <- if(this_journal %in% no_cat_journ) {#plot by manu type for journals w/o categories
  journ_usage_data %>% 
    group_by(manuscript.type, measure.names) %>% 
  summarise(total = sum(measure.values.per.k, na.rm = TRUE)) %>% 
  ggplot()+
  geom_col(aes(x = manuscript.type, y = total, fill = measure.names))+
    scale_y_log10()+
  coord_flip()+
  facet_grid(~measure.names, scales = "free")+
  labs(y = "Total (x100)", x = "Manuscript Type", title = "Article Views by Manuscript Type",
       subtitle = "Includes articles published in the last 12 months")+
  my_theme} else { #plot by category type
    journ_usage_data %>% 
      group_by(category, measure.names) %>% 
      summarise(total = sum(measure.values.per.k, na.rm = TRUE)) %>% 
      ggplot()+
      geom_col(aes(x = category, y = total, fill = measure.names))+
      coord_flip()+
      facet_grid(~measure.names, scales = "free")+
      labs(y = "Total (x100)", x = "Category", title = "Article Views by Category",
           subtitle = "Includes articles published in the last 12 months")+
      my_theme
  }

#plotting over time doesn't work b/c I don't have views over time, I have views per article
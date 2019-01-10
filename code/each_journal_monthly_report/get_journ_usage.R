#Plots of usage stats for journal: YTD bar graphs w. web, PDF, unique users

journ_usage_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_mo(., .$publication.date) %>% 
  filter(measure.names == "Abstract" | measure.names == "HTML" | measure.names == "PDF")

total_views <- journ_usage_data %>% 
  group_by(measure.names) %>% 
  summarise(total = sum(measure.values)) %>%
  ggplot()+
  geom_col(aes(x = measure.names, y = total))+
  labs(title = "Total Article Views", 
       subtitle = "Includes articles published in last 12 months", x = "Metric", y = "Total Views")+
  my_theme_horiz

date_views <- journ_usage_data %>% 
  group_by(floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values)) %>% 
  ggplot()+
  geom_line(aes(x = `floor_date(ymd(publication.date), unit = "month")`, 
                y = total.views, color = measure.names))+
  facet_wrap(~ measure.names, scales = "free_y", ncol = 1)+
  labs(y = "Total Views", x = "Month of Publication", 
       title = "Total Views of Articles by Metric",
       subtitle = "Includes articles published in last 12 months")+
  my_theme_horiz


cat_views <- if(this_journal %in% no_cat_journ) {journ_usage_data %>% 
    group_by(manuscript.type, measure.names) %>% 
  summarise(total = sum(measure.values)) %>% 
  ggplot()+
  geom_col(aes(x = manuscript.type, y = total, fill = measure.names))+
  coord_flip()+
  facet_grid(~measure.names, scales = "free")+
  labs(y = "Total", x = "Manuscript Type", title = "Article Views by Manuscript Type",
       subtitle = "Includes articles published in the last 12 months")+
  my_theme} else {journ_usage_data %>% 
      group_by(category, measure.names) %>% 
      summarise(total = sum(measure.values)) %>% 
      ggplot()+
      geom_col(aes(x = category, y = total, fill = measure.names))+
      coord_flip()+
      facet_grid(~measure.names, scales = "free")+
      labs(y = "Total", x = "Category", title = "Article Views by Category",
           subtitle = "Includes articles published in the last 12 months")+
      my_theme
  }

#plotting over time doesn't work b/c I don't have views over time, I have views per article
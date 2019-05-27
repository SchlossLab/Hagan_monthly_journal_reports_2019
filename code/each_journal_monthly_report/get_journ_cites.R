#Box plot of citations/month by article category type (all, previous 12 mo?)
#Data needed: doi, WoS citation stats, manuscript type

journ_cites_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_to_36_mo(., .$publication.date) %>% 
  filter(measure.names == "Total Article Cites") %>% 
  mutate(category = strtrim(category, 45))

total_cites <- if(this_journal %in% no_cat_journ){
  journ_cites_data %>% 
  group_by(manuscript.type) %>% 
  summarise(total = sum(measure.values)) %>%
  ggplot()+
  geom_col(aes(x = manuscript.type, y = total))+
  labs(title = "Total Article Cites by Manuscript Type", 
       subtitle = "Includes articles published in previous 12 to 36 months",
       x = "Manuscript Type", y = "Total Cites")+
  coord_flip()+
  my_theme
  } else {
    journ_cites_data %>% 
      group_by(category) %>% 
      summarise(total = sum(measure.values)) %>%
      ggplot()+
      geom_col(aes(x = category, y = total))+
      labs(title = "Total Article Cites by Category", 
           subtitle = "Includes articles published in previous 12 to 36 months",
           x = "Category", y = "Total Cites")+
      coord_flip()+
      my_theme}

each_cite <- if(this_journal %in% no_cat_journ){
  journ_cites_data %>%  
  ggplot()+
  geom_boxplot(aes(x = manuscript.type, y = measure.values))+
  coord_flip()+
  labs(y = "Number Cites", x = "Manuscript Type", 
       title = "Article Cites by Manuscript Type",
       subtitle = "Includes articles published in previous 12 to 36 months")+
  my_theme
  }else{
    journ_cites_data %>%  
      ggplot()+
      geom_boxplot(aes(x = category, y = measure.values))+
      coord_flip()+
      labs(y = "Number Cites", x = "Category", 
           title = "Article Cites by Category",
           subtitle = "Includes articles published in previous 12 to 36 months")+
      my_theme}



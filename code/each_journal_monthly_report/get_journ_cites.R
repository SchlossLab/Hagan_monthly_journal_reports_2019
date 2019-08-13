#Box plot of citations/month by article category type (all, previous 12 mo?)
#Data needed: doi, WoS citation stats, manuscript type

journ_cites_data <- data %>% filter(journal == this_journal) %>% 
  filter_12_to_36_mo(., .$publication.date) %>% #restrict to articles published 1-3 yrs prior
  filter(measure.names == "Total Article Cites") %>% #Cites only
  mutate(category = strtrim(category, 45)) #trim category lengths to 45 characters

#barplot of total cites per category/manuscript type
total_cites <- if(this_journal %in% no_cat_journ){ #generate plot for journals w/o categories
  journ_cites_data %>% 
  group_by(manuscript.type) %>% 
  summarise(total = sum(measure.values)) %>% #total cites for each manuscript type
  ggplot()+
  geom_col(aes(x = manuscript.type, y = total))+
  labs(title = "Total Article Cites by Manuscript Type", 
       subtitle = "Includes articles published in previous 12 to 36 months",
       x = "Manuscript Type", y = "Total Cites")+
  coord_flip()+
  my_theme
  } else { #generate plot for journals w/ categories
    journ_cites_data %>% 
      group_by(category) %>% 
      summarise(total = sum(measure.values)) %>% #total cites for each category type
      ggplot()+
      geom_col(aes(x = category, y = total))+
      labs(title = "Total Article Cites by Category", 
           subtitle = "Includes articles published in previous 12 to 36 months",
           x = "Category", y = "Total Cites")+
      coord_flip()+
      my_theme}

#boxplot of cites for each manuscript according to the category/manuscript types
each_cite <- if(this_journal %in% no_cat_journ){ #generate plot for journals w/o categories
  journ_cites_data %>%  
  ggplot()+
  geom_boxplot(aes(x = manuscript.type, y = measure.values))+
  coord_flip()+
  labs(y = "Number Cites", x = "Manuscript Type", 
       title = "Article Cites by Manuscript Type",
       subtitle = "Includes articles published in previous 12 to 36 months")+
  my_theme
  }else{ #generate plot for journals w/ categories
    journ_cites_data %>%  
      ggplot()+
      geom_boxplot(aes(x = category, y = measure.values))+
      coord_flip()+
      labs(y = "Number Cites", x = "Category", 
           title = "Article Cites by Category",
           subtitle = "Includes articles published in previous 12 to 36 months")+
      my_theme}



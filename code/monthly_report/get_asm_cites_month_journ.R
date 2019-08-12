#Box plot of citations/month by article category type (all, previous 12 mo?)
#Data needed: doi, WoS citation stats, manuscript type

cites_month_data <- data %>% 
  filter_12_to_36_mo(., .$publication.date) %>% 
  filter(measure.names == "Article Cites / Month")

journal_cites_month_plot <- cites_Month_data %>% 
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values))+
  labs(x = "Journal", y = "Article Cites/Month",
       caption = "Note: Articles published in the previous 12 to 36 months")+
  my_theme

cat_cites_month_plot <- cites_month_data %>% 
  ggplot()+
  geom_boxplot(aes(x=category, y=measure.values))+
  labs(x = "Category", y = "Article Cites/Month",
       title = " Article Cites Based on Category",
       subtitle = "Articles published in the previous 12 to 36 months")+
  coord_flip()+
  my_theme
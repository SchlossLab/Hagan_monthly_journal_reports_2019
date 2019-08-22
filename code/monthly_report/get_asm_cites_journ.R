#Box plot of citations/month by article category type (all, previous 12 mo?)
#Data needed: doi, WoS citation stats, manuscript type

cites_data <- data %>% 
  filter_12_to_36_mo(., .$publication.date) %>% #articles published between 1 and 3 years previous
  filter(measure.names == "Total Article Cites")

#citations by journal----
journal_cites_plot <- cites_data %>% 
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values))+
  labs(x = "Journal", y = "Article Cites", title = " Article Cites Based on Journal",
       subtitle = "Articles published in the previous 12 to 36 months")+
  my_theme

cat_cites_plot <- category_cites %>% ggplot()+
  geom_boxplot(aes(x=category, y=measure.values))+
  labs(x = "Category", y = "Article Cites",
       title = " Article Cites Based on Category",
       subtitle = "Articles published in the previous 12 to 36 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from 
       AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  coord_flip()+
  my_theme

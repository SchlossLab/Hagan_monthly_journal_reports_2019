#stats over time (smooth dot plot, one line for each stat)

drop.measure.names <- c("Article Cites / Month", 
                        "Published Months", "Total Article Cites")

stats_data <- data %>% 
  filter_12_mo(., .$publication.date) %>% #published in last 12 months
  filter(!(measure.names %in% drop.measure.names)) %>% 
  select(manuscript.number, months.published, measure.names, 
         measure.values, publication.date, category, journal) %>% distinct() %>% 
  mutate(measure.values.per.month = measure.values/months.published,
         measure.values.per.k = measure.values/1000) #divide total measure values by 1000 for plottable values

#all_stats_data %>% group_by(journal, measure.names) %>% #  summarise(total = sum(measure.values))

#usage stats overtime (abstract, html, pdf) for each journal----
asm_time <- stats_data %>% 
  group_by(journal, floor_date(ymd(publication.date), #group articles by journal and publication month
                               unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values.per.k)) %>% #add values per/1000
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, #plot by year and month
                y=total.views, color = measure.names, group = measure.names))+
  facet_wrap( ~ journal, scales = "free_y", shrink = TRUE,
              strip.position = "right", ncol = 1)+
  labs(x = "Month of Publication",
       y = "Views (x1000)",
       title = "Total Article Views by Journal",
       subtitle = "For articles published in the last 12 months",
       color = "Measures")+
  my_theme_leg

#usage stats overtime grouped according to manuscript categories----
pdf_cat <- stats_data %>% 
  filter(measure.names == "PDF") %>% 
  group_by(category, floor_date(ymd(publication.date), 
                                unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values.per.k)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,5000))+
  facet_wrap( ~ category,  
              strip.position = "right", ncol = 4, labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "PDF Views (x1000)",
       title = "PDF Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from 
                  AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme

html_cat <- stats_data %>% 
  filter(measure.names == "HTML") %>% 
  group_by(category, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values.per.k)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,5000))+
  facet_wrap( ~ category, ncol = 4,  
              strip.position = "right", labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "HTML Views (x1000)",
       title = "HTML Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from 
                  AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme

abstract_cat <- stats_data %>% 
  filter(measure.names == "Abstract") %>% 
  group_by(category, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values.per.k)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,10000))+
  facet_wrap( ~ category, ncol = 4, 
              strip.position = "right", labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "Abstract Views (x1000)",
       title = "Abstract Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from 
                  AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme

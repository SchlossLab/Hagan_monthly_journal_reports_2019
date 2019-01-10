#Plot submissions to the journal by month (Jan-Dec) for the current and all years previous, each as a line (would it be better to do a single line from start to end?)

journ_subs_data <- data %>% filter(journal == this_journal) %>% 
  filter(ejp.decision != "Withdrawn") %>% 
  filter(year(approved.date) >= (this_year - 5))

total_years <- year(journ_subs_data$approved.date) %>% unique() %>% length()

sub_trends_plot <- journ_subs_data %>% 
  group_by(year(approved.date), month(approved.date)) %>% summarise(n = n()) %>%
  ggplot()+
  geom_line(aes(x = `month(approved.date)`, y = n, group = as.factor(`year(approved.date)`), 
                color = as.factor(`year(approved.date)`)))+
  scale_color_manual(values = cbbPalette)+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Submissions", color = "Year",
       title = "Submission Trends by Year")+
  my_theme_leg_horiz

#By journal, barplot of submissions for the YTD by month - stacked by submission type (original research vs review vs etc)
#Data needed: submission date for everything since 2013, manuscript type, journal

YTD_subs_data <- data %>% filter(journal == this_journal) %>% 
  filter(year(approved.date) == this_year)

plot_YTD_subs <- YTD_subs_data %>%  
  ggplot()+
  geom_bar(aes(x = month(approved.date)))+
  facet_wrap(~ manuscript.type, scales = "free_y", ncol = 3,
             labeller = label_wrap_gen(width = 13))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  #scale_fill_brewer()+
  labs(x = "Month", y = "Submissions",
       title = "YTD Submissions by Manuscript Type")+
  my_theme

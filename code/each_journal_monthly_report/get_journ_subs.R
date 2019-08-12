#Plot submissions to the journal by month (Jan-Dec) for the current and all years previous, each as a line (would it be better to do a single line from start to end?)

#submissions dataset
journ_subs_data <- data %>% filter(journal == this_journal) %>% 
  filter(ejp.decision != "Withdrawn") %>% #eliminate manuscripts rejected for scope
  filter(year(approved.date) >= (this_year - 3)) #restrict to last 3yrs

#years of submissions to plot
total_years <- year(journ_subs_data$approved.date) %>% unique() %>% length()

#number of submissions grouped by year and month
sub_trends_data <- journ_subs_data %>% 
  group_by(year(approved.date), month(approved.date)) %>% 
  summarise(n = n())


sub_trends_plot <- ggplot(sub_trends_data)+
  geom_line(aes(x = `month(approved.date)`, y = n, 
                group = as.factor(`year(approved.date)`), 
                linetype = as.factor(`year(approved.date)`)))+
  scale_linetype_manual(values = c("dotted", "twodash", "longdash", "solid"))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Submissions", linetype = "Year",
       title = "Submission Trends by Year")+
  my_theme_leg_horiz

#By journal, barplot of submissions for the YTD by month - stacked by submission type (original research vs review vs etc)
#Data needed: submission date for everything since 2013, manuscript type, journal

#data for year-to-date submissions
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
  labs(x = "Month", y = "Submissions",
       title = "YTD Submissions by Manuscript Type")+
  my_theme

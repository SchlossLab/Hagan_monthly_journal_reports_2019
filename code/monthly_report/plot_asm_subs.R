#Submissions per month for current & previous calendar year

#data needed: manuscript number, date submitted (using author approval date as proxy), restrict to last two calendar years

asm_subs <- data %>% filter(version == "0") %>% #only one entry per submission
  separate(approved.date, "submitted.date", sep = " ", extra = "drop") %>% #drop time from author approved date
  separate(submitted.date, c("sub.year", "sub.month", "sub.day"), sep = "-") %>% #split date into ymd
  filter(sub.year == this_year | sub.year == (this_year - 1)) %>% #only those submitted in last 2 years
  distinct()

asm_subs_plot <- asm_subs %>% 
  select(manuscript.number, sub.year, sub.month) %>% distinct() %>% 
  group_by(sub.year, sub.month) %>% 
  summarise(n = n()) %>% #count submissions per month/year
  ggplot()+
  geom_line(aes(x = sub.month, y = n, 
                linetype = sub.year, group = sub.year))+
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(limits = c(0, NA))+ #no upper bound for Y axis
  labs(x = "Month", y = "Submissions", linetype = "Year", 
       title = paste("ASM Submissions by Month for", this_year-1, "and", this_year))+ 
  theme_light()

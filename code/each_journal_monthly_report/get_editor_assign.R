#**Number of original manuscripts assigned to editors**  BY JOURNAL
#  
#  Columns: editor, each month YTD, total, average, & percent of total (ytd). Final two rows are total (for #month across all editors), and cumulative total (all editors, ytd) (includes all article types & withdrawn#/deleted/transferred)
#Data needed: full editor name, journal, date of submission, date assigned to editor, current status (withdrawn#/deleted/transferred), decision

editor_assign_needed_data <- data %>% filter(journal == this_journal) %>% #pull journal of interest
  select(manuscript.number, editor, version, submitted.date) %>% #get relevant data
  separate(submitted.date, c("year", "month", "day"), sep = "-") %>% 
  select(-day) %>% #determine submission year & month
  filter(version == "0") %>% filter(year == this_year) #restrict to first version submitted this year

#df of the number of manuscripts assigned to each editor by month----
by_editor <- editor_assign_needed_data %>% 
  group_by(editor, month) %>% 
  summarise(n = n()) %>% #count manuscripts assigned to each editor, each month
  spread(month, n) %>% as.data.frame() #pull months from single column into one for each month

#add row with total manuscripts assigned per month----
month_sums <- by_editor %>% select(-editor) %>% 
  colSums(., na.rm = TRUE) %>% #calculate total for each column
  as.data.frame() %>% t() %>% #convert to dataframe & transpose to row
  cbind(editor = "Monthly Total", .) %>% rbind(by_editor, .) #add as new row to by_editor

#create empty dataframe to account for future months based on dimensions of month_sums----
empty_months <- tryCatch(
  matrix(data = NA, nrow = nrow(month_sums), ncol = (13 - ncol(month_sums))) %>% 
    as.data.frame(),
  error = function(e) simpleMessage("all months accounted for"))

#update month_sums with empty columns if necessary----
month_sums <- if(ncol(month_sums) != 13) cbind(month_sums, empty_months) else(month_sums)

#calculate total manuscripts assigned to editor in the year----
ed_sums <- month_sums %>% select(-editor) %>% 
  sapply(., as.numeric) %>% rowSums(., na.rm = TRUE) %>% as.data.frame() #convert to numeric & add

#calculate average number of manuscripts assigned per month----
ed_avg <- month_sums %>% select(-editor) %>% 
  sapply(., as.numeric) %>% rowMeans(., na.rm = TRUE) %>% 
  round(., digits = 2) %>% as.data.frame()

#bind total & average calculations to month_sums
editor_assign_summary <- bind_cols(month_sums, ed_sums, ed_avg)

#rename columns
names(editor_assign_summary) <- c("Editor", "Jan", "Feb", "Mar", 
                                  "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                  "Oct", "Nov", "Dec", "Total YTD", "Average")

#add column to calculate the percent of total manuscripts each editor has recieved----
editor_assign_all_summary <- mutate(editor_assign_summary, 
                                    `Percent YTD` = get_percent(
                                      `Total YTD`, 
                                      editor_assign_summary[nrow(editor_assign_summary), 
                                                            ncol(editor_assign_summary)-1]))

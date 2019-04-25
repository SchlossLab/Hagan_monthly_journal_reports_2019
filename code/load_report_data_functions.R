library(tidyverse)
library(lubridate)

data <- read_csv(paste0("processed_data/report_data", this_ym,".csv")) %>% 
  filter(journal != "EC") %>% filter(journal != "genomeA") %>% filter(journal != "CVI")

this_year <- today() %>% year()

this_month <- today() %>% month()

#function to calculate percent and round to 2 decimal places
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}

#function to generate a dataframe that includes all variables, regardless of value (or lack there of)
test_list <- function(all, decision, group){
  
  missing_list <- anti_join(all, decision, by = paste(group)) %>% pull(paste(group)) %>% #identify which items aren't listed
    .[. != "All"|. != "Total"]
  
  missing_df <- if(length(missing_list) == 0) paste("skip") else(
    cbind(missing_list, as.tibble("na")) %>% #generate df of missing items w/ na value
      setNames(., c(paste(group), colnames(decision)[2])) #rename columns (setNames allows conditional naming)
  )
  
  total <- decision %>% filter(.[1] == "All"|.[1] == "Total") #get the total row by itself
  
  corrected_decision <- if(str_detect(missing_df, "skip")) decision else(
    rbind(decision, missing_df) %>% #bind missing journals to decision table
      arrange_(group) %>% #put first column in alphabetical order
      filter(.[1] != "All") %>% filter( .[1] != "Total") %>% rbind(total)) #move the total/all row to the bottom
  
  return(corrected_decision)
}

#function to restrict selection to manuscripts with date X that occurs in the last 12 months
filter_12_mo <- function(x, date){
  filter(x, date %within% interval(start = (today() - months(12)), end = today()))
}

#function to restrict selection to manuscripts with date X that occurs in the previous 12 to 36 months -- relevant cites timeframe
filter_12_to_36_mo <- function(x, date){
  filter(x, date %within% interval(start = (today() - months(36)), end = today() - months(12)))
}
